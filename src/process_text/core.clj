(ns process-text.core
  (:gen-class)
  (:require [net.cgrand.enlive-html :as html])
  (:require [org.httpkit.client :as httpkit])
  (:require [clojure.pprint :as pprint])
  (:require [clj-http.client :as client])
  (:require [cheshire.core :as cheshire])
  (:require [monger.core :as mg])
  (:require [monger.collection :as mc])
  (:import [com.mongodb MongoOptions ServerAddress])
  )

(defn fetch-url [url]
	(try 
		(html/html-resource (java.net.URL. url))
	  (catch Exception e 
		(do 
		  (println "caught exception: " (.getMessage e))
		  nil ; nil on error
		))
	)
)

(defn load-word-file-as-list [f]
	"Load a file and return all of the words as a sequence."
	(with-open [rdr (clojure.java.io/reader f)]
		(clojure.string/join "\n" (line-seq rdr))  
	))

(defn remove-punc 
	"Remove punctuation from the given string."
	[x] 
	(clojure.string/trim-newline 
		(clojure.string/replace
			(clojure.string/replace 
				(clojure.string/trim-newline x) 
				#"[\|„“\"”“\.\\,=+%()\$!?<>;'、，،]"   ; comma varients https://en.wikipedia.org/wiki/Comma
				" ")
			"\n" ; second replace
			"")))
			
(defn ignore-word-filter
	"returns true if a given word should be ignored, or false if not"
	[inw]
	(let [strinw (str inw)] 
	  (println strinw)
	  (or (clojure.string/blank? strinw)
		(clojure.string/includes? strinw "]")
		(clojure.string/includes? strinw "[")
		(clojure.string/includes? strinw "}")
		(clojure.string/includes? strinw "{")
		(clojure.string/includes? strinw ":")
	)))

(defn url-encode [s] (java.net.URLEncoder/encode s "UTF-8"))
			
(defn extract-wiki-text 
	"given a wikipedia article in enlive form, get only the article text.
	If there is an error getting the text, then return an empty string. "
	[text]
	(if (nil? text)
	  "" ; empty string on nil value
	  (do (defn iter-down
		[t] 
		(if (seq? t)
			(concat (map iter-down t))
			(if (nil? (get t :content))
				t
				(iter-down (get t :content))
			)
		)
	  )
	  (filter 
	    (fn [n] ; get rid of invalid strings
		  (or
			(not (clojure.string/blank? n)) ; it is a blank string
			(< (count n) 1) ; size is less than 1 
		  )
		)
		(map ; this map results in a list of words for the article
			clojure.string/lower-case 
			(map clojure.string/trim (clojure.string/split 
			  (remove-punc 
				(clojure.string/join " " 
				  (flatten (iter-down 
					(flatten 
					  (html/select text [:div.mw-body-content :div.mw-parser-output :p])
					) 
			      )) ; making a list of all the words
				  ) ; join
				) 
			  #" "
			  )
			)
		)
	  ))
	)
)
	
	
(defn wcount 
	"count the number of occurances of each word. Returns a dictionary."
	[wlist] ; a list of words
	(let [mcount_input {}]
	  (loop [i 0 mcount mcount_input]
		(if (>= i (count wlist))
		  mcount
		  (if (ignore-word-filter (nth wlist i)) ; if the word should be ignored...
		    (recur (inc i) mcount) ; then don't add to the word count.
		    (recur (inc i) (if (nil? (get mcount (nth wlist i)))
			  (assoc mcount (nth wlist i) 1)
			  (update mcount (nth wlist i) inc)
		    ))
		  )
		)
	))
)

(defn wcount-merge
	"given a list of word count maps, merge them all together and sum the counts of common words"
	[wlists]
	(merge-with + wlists)
)


(defn wcount-merge-old
	"given a list of word count maps, merge them all together and sum counts"
	[wlists]
	;(println wlists)
	(reduce 
		(fn [wlist1 wlist2] 
			(loop [result wlist1 q wlist2]
				(if (= 0 (count q))
					result
					(let [ nextkey (first (keys q)) rmq (dissoc q nextkey)]
						(if (nil? (get result nextkey))
							(recur (assoc result nextkey 
								(get q nextkey)) rmq )
							(recur (assoc result nextkey 
								(+ (get result nextkey) (get q nextkey))) rmq )
						)
					)
				)
			)
		)
	wlists)
)

(defn wcount-normalize
	"given a word count map, return the same word count map but normalized."
	[wcount]
	(let [words (keys wcount) superCountSum 
		(loop [k 0 resultSum 0]
		  (if (>= k (count wcount))
		    resultSum
			(recur (inc k) (+ resultSum 
			  (get wcount (nth words k))
			))
		  )
		)
	  ]
	  (loop [k 0 resD {}]
	    (if (>= k (count (keys wcount)))
		  resD
		  (recur (inc k) (assoc resD (nth (keys wcount) k)
		    (/ (get wcount (nth (keys wcount) k)) superCountSum)
		  ))
		)
	  ) ; TODO
	)
)

(defn word-in-model 
	"get the chance that a word was generated using the given model m."
	[m w] ; model, word
	(if (contains? m w)
		(get m w 0) ; return a 0 whenit is not found.
		0))

(defn count-in-doc
	"get the count of a word w from a document d."
	[d w] ; model, word
	(if (contains? d w)
		(get d w 0)
		0))

		
(defn topic-probs
	[
		documentBows ; a list of all documents as bag-of-words (map string to count)
		vocabulary ; list of all words possible
		topicModelsInitial ; vector of models (map string to probability)
		bgModel ; since map (string to proability) for background words
		bgProb ; chance of using the background model.
	]
	(let ; what is being defined and constant? 
	  [
		doc-word-matrix ( ; matrix where rows = document, columns = word, value = count.
		  mapv (fn [docBow]
			(mapv 
			  (fn [word]
			    (count-in-doc docBow word)
			  )
			vocabulary)
		  ) documentBows)
		doc-word-count (fn [docIndex wordIndex] ; helper func for above matrix
			(nth (nth doc-word-matrix docIndex) wordIndex))
		topicModelIndices (range (count topicModelsInitial))
		documentBowsIndices (range (count documentBows))
		vocabularyIndices (range (count vocabulary))
	  ]
	(loop ; what is being defined to update at each step?
	  [
	    n 0 ; what iteration it is on.
		
		; initial topic models and probability of topics... will be updated in M-step.
		topicModels (mapv ;topicModels[topic][word] = chance of generating that word with topic.
		  (fn [topicIndex]
			(mapv 
			  (fn [wordIndex]
				(word-in-model (nth topicModelsInitial topicIndex) (nth vocabulary wordIndex))
			  )	(range (count vocabulary)) )
		  ) topicModelIndices)
			  
		piDocGenByTopic ( ; [document][topic] = probability doc was generated by topic
		  mapv (fn [docBowIndex]
		    (mapv (fn [topicIndex]
			  (/ 1 (count topicModels)) ; uniform distribution initially
			) topicModelIndices)
		  ) documentBowsIndices)
	  ]
	  (println "n =" n)
	  ;(println "piDocGenByTopic: " piDocGenByTopic)
	  ;(println "topicModels: " topicModels)
	  (let 
	    [ 
		; E-step hidden variables.
		zBgModelProbs ( ; [document][word] = probabiity it was generated by bg model.
		  mapv (fn [docBowIndex]
			(mapv 
			  (fn [wordIndex]
			    (let [word (nth vocabulary wordIndex)] 
				(if (contains? bgModel word)
			    (/ 
			      (* bgProb (word-in-model bgModel word))
				  (+
				    (* bgProb (word-in-model bgModel word))
				    (* 
					  (- 1 bgProb)
				      (reduce +  ; summation
					    (map (fn [topicIndex]
						  (*
						    (nth (nth piDocGenByTopic docBowIndex) topicIndex) ; probability of document generated by this topic
							(nth (nth topicModels topicIndex) wordIndex)
						  )
						) topicModelIndices)
					  )
				    )
				  )
				)
				0 ; if the word is not in the background model, then 0% of being generated.
				))
			  )
			vocabularyIndices)
		  ) documentBowsIndices)
		
		  zProbs ( ; [document][word][topic] = probabiity word in document was generated by topic.
		  mapv (fn [docBowIndex]
			(mapv 
			  (fn [wordIndex]
			    (let [word (nth vocabulary wordIndex)] 
			    ;(println "wordIndex =" wordIndex "word =" word)
			    (mapv 
				  (fn [topicIndex]
				    ;(println (nth topicModels topicIndex))
					(let [numer (reduce +  ; summation to normalize over all topics
					    (map (fn [topicIndexInner]
						  (*
							(nth (nth piDocGenByTopic docBowIndex) topicIndexInner) 
							(nth (nth topicModels topicIndexInner) wordIndex)
						  )
						) topicModelIndices)
						)]
				    (if (== numer 0)
					  0
					  (/ 
					    (* 
					      (nth (nth piDocGenByTopic docBowIndex) topicIndex) ; chance doc is generated by topic
						  (nth (nth topicModels topicIndex) wordIndex) ; chance of word being generated by topic
					    )
					    numer				  
					  )
					))
				  )
				topicModelIndices))
			  )
			vocabularyIndices)
		  ) documentBowsIndices)
	    ]
		;(pprint/pprint zBgModelProbs)
		;(pprint/pprint zProbs)
		(if (>= n 10) ; how many iterations there are.
		  piDocGenByTopic ; output when we're done.
		  (recur 
		    (inc n)
			
			; topicModels
		    (mapv ; [topic][word] = chance of generating that word with topic.
			  (fn [topicIndex]
			    (mapv 
				  (fn [wordIndex]
				  (let [denom (reduce + ; summation over documents
					    (mapv 
					      (fn [docBowIndex]
					  	    (*
						      (doc-word-count docBowIndex wordIndex)
							  (- 1 (nth (nth zBgModelProbs docBowIndex) wordIndex) )
							  (nth (nth (nth zProbs docBowIndex) wordIndex) topicIndex)
						    )
						  )
					    documentBowsIndices)) ]
				  (if (== denom 0)
				    0
				    (/
					  denom
					  (reduce + 
					    (mapv 
					      (fn [innerWordIndex] ; normalize over vocabulary
							(reduce + ; summation over documents
							  (mapv 
								(fn [docBowIndex]
								  (*
									(doc-word-count docBowIndex innerWordIndex)
									(- 1 (nth (nth zBgModelProbs docBowIndex) innerWordIndex) )
									(nth (nth (nth zProbs docBowIndex) innerWordIndex) topicIndex)
								  )
								)
							  documentBowsIndices)
							))
						vocabularyIndices )
					  )
				    )))
				  )	vocabularyIndices )
			  ) topicModelIndices) ; end topicModels
			
			; piDocGenByTopic
		    (mapv (fn [docBowIndex] ; [document][topic] = probability doc was generated by topic
				(mapv (fn [topicIndex]
				  (let [denom 
					  (reduce + 
					    (mapv 
						  (fn [wordIndex]
						    (*
							  (doc-word-count docBowIndex wordIndex)
							  (- 1 (nth (nth zBgModelProbs docBowIndex) wordIndex) )
							  (nth (nth (nth zProbs docBowIndex) wordIndex) topicIndex)
						    )
						  )
						  vocabularyIndices))
					] 
				    (if (== denom 0) 
					  0
					  (/
					    denom
						(reduce + 
						  (mapv 
						    (fn [outerTopicIndex]
						      (reduce + 
							  (mapv 
							    (fn [wordIndexInner]
								  (*
								    (doc-word-count docBowIndex wordIndexInner)
								    (- 1 (nth (nth zBgModelProbs docBowIndex) wordIndexInner) )
								    (nth (nth (nth zProbs docBowIndex) wordIndexInner) outerTopicIndex)
								  )
							    )
							    vocabularyIndices))
						    ) 
						  topicModelIndices))
					  )
					)
				  )
				) topicModelIndices)
			  ) documentBowsIndices) ; end piDocGenByTopic
		  ))
		) ; end inner let
	) ; end loop
  ) ; end first let
)


(defn write-language-model 
	"given a file name and a language model as a probability distribution,
	 write the data in the model to file. An additional description line is added."
	[fname m desc]
	(println "Writing to " fname)
	(with-open [w (clojure.java.io/writer fname)]
	  (.write w "#comment# ") 
	  (.write w desc) 
	  (.write w "\n") 
	  (loop [i 0]
		(if (>= i (count (keys m)))
		  (.close w)
		  (do 
			(.write w (str (nth (keys m) i))) 
			(.write w ",") 
			(.write w (str (get m (nth (keys m) i))) )
			(.write w "\n")
		  (recur (inc i)))
		)
	))
)


(defn read-language-model 
  "given a file name, read the language model"
  [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
	(loop [m {}]
	  (let [nextLine (.readLine rdr)] 
	     (if (nil? nextLine) ; test eof
			   ; end if test to see if the line is valid to read in
			m
			(let [nextTerm 
					(clojure.string/replace 
					  (if (and (not (clojure.string/starts-with? nextLine "#comment#")) ; don't read two lines if it is a comment line
						  (nil? (re-find #"((.)*\n?,[0-9]+(\/[0-9]+)?)" nextLine)))
					    (clojure.string/join #"" [nextLine (.readLine rdr)]) ; append the next line to this one if invalid
					    nextLine ; if the line is valid, then don't append the next line.
				 	  ) #"[\r\n]" " ") ; replace new lines with spaces
				 ]
			  (if (and (not (clojure.string/blank? nextTerm)) ; ignore blank terms
					  (clojure.string/includes? nextTerm ",")) ; double check comma present
			    (let [split-commas (clojure.string/split nextTerm #",")
					  split-ln  ; last token is the number while the remaining is the term. This is for terms that contain commas.
						(list (clojure.string/join #"," (drop-last split-commas)) (last split-commas))
					  split-val (clojure.string/split (nth split-ln 1) #"/") ; expects rational number.
					]
				 (recur (assoc m (nth split-ln 0) 
				   (if (= 2 (count split-val)) ; if it is a rational number or not (splitting by / gave two parts)
					  (/ (Integer/parseInt (nth split-val 0)) (Integer/parseInt (nth split-val 1)) ) ; if so, divide the two numbers
				     (Integer/parseInt (first split-val)) ; otherwise just include the number
				  )
			    )))
			  (recur m) ; if the term was valid, continue iterating and reading.
			))
		 )))
  ))

	
(defn wiki-url-to-document 
	"Given a wikipedia title and language, convert the article to a document containing the words"
	[v] ;[title lang-short, parent, level ]
	(let [title (nth v 0) 
		  lang-short (nth v 1)	
		  url (clojure.string/join "" [ "https://" lang-short ".wikipedia.org/wiki/" (url-encode title) ])]
		; (println (clojure.string/join "" ["Getting wikipedia article at " url]))
		(extract-wiki-text (fetch-url url))))

; Normal unexpanded version
(defn wiki-url-to-bow 
	"Given a wikipedia title and language, convert the article to a Bag of Words with counts."
	[v] ;[title lang-short, parent, level ]
	(wcount (wiki-url-to-document v)))

; Experimental version - expanding the documents with bilingual dictionary.
(defn wiki-url-to-bow-expand
	"Given a wikipedia title and language, convert the article to a Bag of Words with counts.
	source-dict is the article language to the target language. target-dict is the target 
	language to the article language. Both dictionaries are functions that return a list
	given a single word. Must also pass in connection to database with dictionaries."
	[v source-dict target-dict ignore-pred] ;[title lang-short, parent, level ]
	(let [srcdoc (wiki-url-to-document v)]
	  ; take source document and expand each word, adding the result to a resulting document.
	  (let [ 
	    number-of-connections 10 ; guess how many threads that will run
	    number-of-words-per-thread (inc (int (/ (count srcdoc) number-of-connections))) ; round up
		resultdoc 
	    (for [word-colls (partition number-of-words-per-thread srcdoc)] ; for each word in the source doc... 
			(future 
				(let [ ; each collection of words has its own future/thread
				  md-conn (mg/connect) ; each thread has its own connection.
				  md-dictionaries (mg/get-db md-conn "dictionaries")
				] 
				(let [res-colls 
					(for [word word-colls] ; expand this word
					  (doall 
						(println word)
						(if (not (ignore-pred word))
						  (let [twords (source-dict md-dictionaries word)]
							(if (nil? twords) 
							  (list word) ; don't change the word if it has no translation found.
							  (let [res-words (do 
								(flatten 
								  (map ; expand the translation list with source translations
									(fn [tw]
									  (target-dict md-dictionaries tw))
								  twords) 
								))] 
								(conj res-words word) ; add the original word to the result.
							  )
							)
						  )
						  (list word) ; just return the word if it is an ignored one.
					  )) ) ] ; map for each word in this thread's collection
			  (doall 
			    (mg/disconnect md-conn) ; need to close connection to free it waiting threads.
			    res-colls)
			  )
		    )); end future 
		)	]
		(let [ resultdocNotified 
			;(flatten 
			  (map 
				(fn [n]
				  (if (future? n)
				    @n   ; wait for the future if it is a future.
					n)) 
			   resultdoc)
			   ;) 
			 ]
		  (println resultdoc)
		  (wcount resultdocNotified)
		)
	  )
	))
	
		
(defn wiki-url-to-links-list
	"Given a wikipedia title and language, get the first 10 titles of articles that are links to the given article.."
	[v] ;[title lang-short, parent, level]
	(let [
	    title (nth v 0) 
	    lang-short (nth v 1)
		url-base (clojure.string/join "" [ "https://" lang-short   ".wikipedia.org/w/api.php?action=query&prop=links&format=json&titles=" (url-encode title) ])
	  ] 
		(loop [
			  all-links ()
			  url url-base
			]
			(let [jres (cheshire/parse-string 
						(get (client/get url 
							{:cookie-policy :none :content-type "text/html; charset=utf-8"}) 
						:body)
					  )
			  ]
			  (println url)
			  (let [
					adding-links (map ; adding-links are the links that were grabbed this last step. 
						(fn [articleName] ; turn each article name into the standard format.
						  [ articleName lang-short title (inc (nth v 3)) ]
						) 
						(remove 
						  (fn [n] ; remove titles that have a : in them.
							(clojure.string/includes? n ":")
						  )
							(distinct 
							  (flatten 
								(map 
								  (fn [n] 
									(clojure.string/replace (get n "title") #" " "_" )
								  )
								  (get 
									(nth (first (get (get jres "query") "pages")) 1) 
									"links")
								)
							)))
					    )
					continue-str (if (nil? (get jres "continue"))
									nil ; don't continue if no continue
								    (get (get jres "continue") "plcontinue") ; otherwise, get continue text
								)
				  ]
				(if (nil? continue-str)
				   (concat all-links adding-links) ; just return the list of links when not continuing
				   (recur 
					  (concat all-links adding-links) ; update next step list of links
					  (clojure.string/join "" [url-base "&plcontinue=" continue-str] ) ; add continue query to the url-base 
				   )
				)
			  )
			)
		)
	))
	

(defn expand-wiki-list 
	"Given a list of wikipedia titles, return all titles of articles
	that the given wikipedia titles have at least one link to."
	[wlist]
	;(println "Expanding " (count wlist) " links")
	(distinct (reduce concat (map wiki-url-to-links-list wlist))))


; These two functions use the local mongoDB database.
; The schema is as follows: objects have two fields including
; the _id, word (string) and translations (array of strings). 
; There will be an index over the field "word"

(def expand-amount 3)

(defn en-to-de-dict 
	"Given a word in english, return a list of
	possible german translations of that word."
	[md-db word]
	;(println "translating " word)
	; try to find the word by itself and with "to " in front of it for possible verbs 
	(let [ret (mc/find-maps md-db "en_de_dict" { :word word })
		 ret_with_to (mc/find-maps md-db "en_de_dict" { :word (clojure.string/join #" " ["to" word]) })]
		; convert the map into a list of all possible translations
		(take expand-amount (distinct (concat 
		  (flatten (map (fn [w] (get w :translations)) ret))
		  (flatten (map (fn [w] (get w :translations)) ret_with_to))
		)))
	)
)

(defn de-to-en-dict
	"Given a word in german, return a list of
	possible english translations of that word."
	[md-db word] 
	;(println "translating " word)
	(let [ret (mc/find-maps md-db "de_en_dict" {:word word})]
		; list of maps to lists of lists that are flattened... 
	    (take expand-amount (distinct   ; need to reduce how much there is to make it more computationally feasible!
	      (flatten (map (fn [w] (get w :translations)) ret))
		)) ; limit the amount taken
	)
)
	
	
(defn load-dictionary-files
	"Given the list file names for dictionaries, load them all into a map 
	that links words to a list of possible translations for that word."
	[& fnames]
	(println "Loading " fnames)
	(let [raw-lines (flatten (loop [i 0 l (list)]
	  (if (>= i (count fnames))
		l
		(recur (inc i) (conj l 
			(with-open [rdr (clojure.java.io/reader (nth fnames i))]
			  (doall (line-seq rdr))
			))
		))
	  ))]
	  ;(println "\n\nraw-lines:")
      ;(println raw-lines)
	  ; raw-lines into them map
	  (loop [i 0 m {}]
	    (if (>= i (count raw-lines))
		  m
		  (recur (inc i)
		    (let [currln (str (nth raw-lines i))
				  split-first (clojure.string/split currln #"\|")
				  split-second (clojure.string/split (nth split-first 1) #"&")] 
				(assoc m (first split-first) split-second)) ; map the first word to list of possible translations
			)
		)
	  )
    )
)

(defn is-word-prep-or-article-en 
	"Return true or false whether the given english 
	word is an article or preposition."
	[inw]
	(let 
	  [w (clojure.string/trim inw)]
	  (println "testing " w " english")
	  (let [prepositions (clojure.string/split (load-word-file-as-list "prepositions-en.csv") #"\n") 
	      articles (list "a" "an" "the")]
		   (or 
			  (< 1 (count w))
			  (some (partial = w) prepositions)
			  (some (partial = w) articles)
			)
		)
	))

	
(defn is-word-prep-or-article-de 
	"Return true or false whether the given german 
	word is an article or preposition."
	[inw]
	(let 
	  [w (clojure.string/trim inw)]
	(println "probieren " w " deutsch")
	(let [prepositions (clojure.string/split (load-word-file-as-list "prepositions-de.csv") #"\n") 
	      articles (list "ein" "eine" "einem" "einer" "eines" "der" "die" "das" "dem" "den" "des")]
		   (pprint/pprint prepositions)
		   (pprint/pprint articles)
		   (or 
			  (< 1 (count w))
			  (some (partial = w) prepositions)  ; https://www.programming-idioms.org/idiom/12/check-if-list-contains-a-value/807/clojure
			  (some (partial = w) articles)
			)
		)
	))

	
(defn build-topic-models-main
	"build topic models main function"
	[args]
	; format of the articles: [ ["title" "lang-short"], parent, depth ]
   (def starting-articles-to-fetch 
     (map
		(fn [n] [ [(first n) (nth n 1)] (list n)])  ; 2 item vector of a list and the title-language pair
		  (list 
		    ; English topic model articles
			;[ "Sport" "en" nil 0 ]
		    ;[ "Politics" "en" nil 0 ]
		    ;[ "Science" "en" nil 0 ]
		    ;[ "Business" "en" nil 0 ]
		    
			; German topic model articles
			;[ "Sport" "de" nil 0 ]
		    ;[ "Politik" "de" nil 0 ]
		    ;[ "Wissenschaft" "de" nil 0 ]
		    ;[ "Geschäft_(Wirtschaft)" "de" nil 0 ] ; NOT a wikipedia inter-language link
			
			; old testing articles
			[ "Kumo_to_Tulip" "en" nil 0]
			;[ "Toran" "en" nil 0]
			;[ "Fruit" "en" nil 0 ]
		    ;[ "Stachytarpheta_svensonii" "en" nil 0 ]
		   ; [ "Frucht" "de" nil 0 ]
		    ;[ "ثمرة" "ar" nil 0 ] ; - this is not working yet :(
		  )))
	(pprint/pprint starting-articles-to-fetch)

	;(println "ثمرة")
	;(println (.getBytes "ثمرة"))
	; https://docs.oracle.com/javase/7/docs/api/java/net/URLEncoder.html#encode(java.lang.String,%20java.lang.String)
	;(println (java.net.URLEncoder/encode "ثمرة" "UTF-8") )
	;(println (new java.lang.String (.getBytes "ثمرة") "US-ASCII")) ; encode it to ascii characters. 
	;(println "%E6%9E%9C%E5%AE%9E")
	;(comment
	;(println ( stringify-bytes "果实"))
	(def link-expand-amount 0) ; how many adjacent articles to add for each topic model.
	
	(def prop-links 
		(map 
		  (fn [inlistv]
		    [ (first inlistv) (loop [i 0 elist (nth inlistv 1)]
			  (if (>= i link-expand-amount) ; how much to expand it?
				(do 
				  (println "Done getting links")
				  (println (count elist))
				  elist)
				(recur (inc i) (reduce concat (list elist (expand-wiki-list elist))))
			  )) 
			] )
		starting-articles-to-fetch)
	)
	
	; new article format: [ ["title" "lang-short"] {word-count-map}]
	
	; whether or not to expand or not expand when making the topic models (experimental)
	(def expand true) 
	
	(println "Building BOWs. Expand = " expand)
	; making the Bows with expansion using a dictionary.
	(def articleTopicsBow
	  (if expand 
	    (map ; expand
		  (fn [linklsv] 
			[(first linklsv) ; starting element is the root article - vector with title and language
			  (map  ; second part is the expanded bow
				(fn [n] ; n is the link
					(if (= (nth n 1) "en")
						(wiki-url-to-bow-expand n 
							en-to-de-dict 
							de-to-en-dict 
							(fn [w]
							  (or (is-word-prep-or-article-en w)
								(ignore-word-filter w))
							))
						(if (= (nth n 1) "de")
						  (wiki-url-to-bow-expand n 
							de-to-en-dict 
							en-to-de-dict 
							(fn [w]
							  (or (is-word-prep-or-article-de w)
								(ignore-word-filter w))
							))
						   (wiki-url-to-bow n) ; can't expand that language, so do normally
						)
					)
				)
				(nth linklsv 1))
			] 
		  ) prop-links) ; end if expanded
		  
		(map    ; else do not expand
		  (fn [linklsv] 
			[(first linklsv) ; vector with title and language
			  (map  ; second part is the expanded bow
				(fn [n] ; n is the link
					(wiki-url-to-bow n)
				)
				(nth linklsv 1))
			] 
		  ) prop-links)
	  ) ; end if
	)
	
	(def file-name-pre 
	  (if expand
		"topic-model-expand-"
		"topic-model-"
	  )
	)
	
	(println "Building the language model")

	; divide the merged word counts by how many word count
	; lists each word appears in the origina list of word counts. 
	(def mergedWordCountsTfidf
	  (map 
	    (fn [linklsv] 
		  [(first linklsv) 
			(let [ 
					merged-counts (wcount-merge (nth linklsv 1))
					unmerged-list (nth linklsv 1)
				] 
				(if (> (count merged-counts) 1) ; if there is more than one document count...
					(reduce-kv
					  (fn [m k v]
						(let [
							; num-docs-in must be at least 1, otherwise it would not be in final merged list
							num-docs-in (reduce + ; sum the result
								(map 
								  (fn [unmerged-list-elem]
									(if (nil? (get unmerged-list-elem k))
									  0 ; not present
									  1 ; present
									)
								  )
								  unmerged-list))
							]
						   (assoc m k (/ v num-docs-in)) 
						)
					  ) 
					{} merged-counts)
					(first merged-counts) ; otherwise if there is only one map just return it
				)
			)]
		) 
	  articleTopicsBow)
	)

	(defn write-topic-models [] (map 
	  (fn [titleModelPair]
	    (pprint/pprint (str (nth titleModelPair 0)))
		(write-language-model (clojure.string/join #"" [file-name-pre 
			(str (clojure.string/join #"-" (reverse (nth titleModelPair 0)))) ".lm"] )
			(wcount-normalize (nth titleModelPair 1)) ; the first element should be the merged word count list.  
			(clojure.string/join #" " ["Topic model for" (str (nth titleModelPair 0))] ))
	  )
	  mergedWordCountsTfidf))
	  
	(println (write-topic-models)) ; this actually runs all the things
	
	; code to make the background model.
	;(def google-word-list (clojure.string/split (load-word-file-as-list "google-10000-english.txt") #"\n") )
	;(def google-bg-model (wcount-normalize (wcount google-word-list)))
	;(write-language-model "google-common-words.lm" google-bg-model "The 10000 most common english words")
) ; end build topic model function


(defn evaluate-topic-main
	"build topic models main function"
	[args]
	(println args)
	(let 
	  [
 	    bgfile (first args) ; background model file
	    infiles (rest args) ; & input files
	  ]
		(println "in files names:")
		(pprint/pprint infiles)
		(println "background file name:")
		(pprint/pprint bgfile)
		; new article format: [ ["title" "lang-short"] {word-count-map}]
		
		; TODO: This has to do with calculating results using existing language models. 
		; need to redefine the documentsBOW variable here. 
		(def documentsBows
		  (map 
		    (fn [file] ; load file, remove punctuation, and make the word count map
			   (wcount 
			     (map 
				   remove-punc 
			       (clojure.string/split (load-word-file-as-list file) #"\n")))
			)
		  infiles)
		)
		
		(pprint/pprint documentsBows)
		
		; need to load in testModels again. 
		(def testModels
		   [
			  (read-language-model "topic-model-expand-en-Politics.lm")
			  (read-language-model "topic-model-expand-en-Sport.lm")
		   ])
		
		(def bgModel 
		  (read-language-model bgfile)) ; load background model
		(def bgModelProb 0.7)
		(def vocabulary 
		  (distinct 
			(flatten 
			  [
			    (map keys documentsBows) 
				(keys bgModel) 
				(flatten 
				  (map keys testModels)
				)
			  ]
			)))
		
		(pprint/pprint 
			(topic-probs documentsBows vocabulary testModels bgModel bgModelProb))
		
		
		; code to make the background model.
		; (def google-word-list (clojure.string/split (load-word-file-as-list "google-100-english.txt") #"\n") )
		; (def google-bg-model (wcount-normalize (wcount google-word-list)))
		; (write-language-model "google-common-words-small.lm" google-bg-model "The 10000 most common english words")
	)
)

(defn -main
  "main function."
  [& args]
  ; first argument is the mode
  ; b is building
  ; e is evaluating
  (do 
	(println args)
	(if (= (first args) "b")
	  (build-topic-models-main (rest args))
	  (if (= (first args) "e") 
		(evaluate-topic-main (rest args))
		nil
	  )
  ))
)
