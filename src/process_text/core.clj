(ns process-text.core
  (:gen-class)
  (:require [net.cgrand.enlive-html :as html])
  (:require [org.httpkit.client :as httpkit])
  (:require [clojure.pprint :as pprint])
  (:require [clj-http.client :as client])
  (:require [cheshire.core :as cheshire])
  )

(defn fetch-url [url]
	(html/html-resource (java.net.URL. url)))  ;TODO have a case for handling exceptions.

; NOT DONE YET
(defn load-word-list [f]
	"Load a file and return all of the words as a sequence."
	(with-open [rdr (clojure.java.io/reader f)]
		(clojure.string/join "\n" (distinct (line-seq rdr)))  
	))

(defn remove-punc 
	"Remove punctuation from the given string."
	[x] 
	(clojure.string/trim-newline 
		(clojure.string/replace
			(clojure.string/replace 
				(clojure.string/trim-newline x) 
				#"[\"\.\\,=+%()\$!?<>;']" 
				" ")
			"\n" ; second replace
			"")))

(defn extract-wiki-text 
	"given a wikipedia article in enlive form, get only the article text."
	[text]
	(defn iter-down
		[t] 
		(if (seq? t)
			(concat (map iter-down t))
			(if (nil? (get t :content))
				t
				(iter-down (get t :content))
			)
		)
	)
	(filter (fn [n] 
				(and
				(not (clojure.string/blank? n))
				(not (nil? (re-matches #"[0-9a-zA-Z]*" n)))
				(not (nil? (re-matches #"[a-zA-Z]*" n)))
			))
		(map clojure.string/lower-case (map clojure.string/trim (clojure.string/split 
			(remove-punc (clojure.string/join " " (flatten (iter-down 
				(flatten (html/select text [:div.mw-body-content :div.mw-parser-output :p])) 
			)))) ; this is the article
			#" "
	)))))
	
	
(defn wcount 
	"count the number of occurances of each word. Returns a dictionary."
	[wlist]
	(let [mcount_input {}]
	  (loop [i 0 mcount mcount_input]
		(if (>= i (count wlist))
		  mcount
		  (recur (inc i) (if (nil? (get mcount (nth wlist i)))
			(assoc mcount (nth wlist i) 1)
			(update mcount (nth wlist i) inc)
		  ))
		)
	))
)

(defn wcount-merge
	"given a list of word count maps, merge them all together and sum counts"
	[wlists]
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

(defn wiki-url-to-bow 
	"Given a wikipedia title and language, convert the article to a Bag of Words with counts."
	[v] ;[title lang-short]
	(let [title (nth v 0) 
		  lang-short (nth v 1)	
		  url (clojure.string/join "" [ "https://" lang-short ".wikipedia.org/wiki/" title ])]
	(wcount (extract-wiki-text (fetch-url url)))))

(defn wiki-url-to-links-list
	"Given a wikipedia title and language, get the first 10 titles of articles that are links to the given article.."
	[v] ;[title lang-short]
	(let [title (nth v 0) 
	  lang-short (nth v 1)	
	  url (clojure.string/join "" [ "https://" lang-short ".wikipedia.org/w/api.php?action=query&prop=links&format=json&titles=" title ])]
		(let [jres (cheshire/parse-string (get (client/get url {:cookie-policy :none}) :body))]
		  (map 
		  (fn [n] (vector n "en")) 
		  (distinct 
			(flatten 
			  (map 
				(fn [n] (clojure.string/lower-case (get n "title"))) 
				(get (nth (first (get (get jres "query") "pages")) 1) "links")))))
		)
	))

(defn -main
  "main function."
  [& args]
  (do 
  
	(def urls-to-fetch (list
		[ "Earth" "en" ]
		[ "Fruit" "en" ]
		[ "Life" "en" ]
		[ "WCNP" "en" ]
		[ "My_Shanty,_Lake_George" "en" ]
	))
	
	(def da-links (distinct (reduce concat (map  wiki-url-to-links-list  urls-to-fetch))))
	
	;(pprint/pprint  da-links)
	
	(def da-links-2 (distinct (reduce concat (map  wiki-url-to-links-list  da-links))))

	(pprint/pprint (wcount-merge (map wiki-url-to-bow da-links-2)))
	
	;(def many-wcount-maps (map  wiki-url-to-bow  urls-to-fetch))
	;(def blah (wcount-merge many-wcount-maps))
	;(pprint/pprint blah)
	
	;(def html-root (nth text 1))
	
	; (def text (with-open [rdr (clojure.java.io/reader (first args))]
	;	(clojure.string/join "\n" (line-seq rdr))))
	
	;(def anchors-in-body (html/select text [:body :a]) )
	;(def divs-with-juicer-class (html/select text [:body :div.juicer ]) )
	
	;(def select-word-header-cambridge (html/select text [:div.cdo-section-title-hw]))
	
	; (def words (filterv (fn [x] (not (= x ""))) (mapv remove-punc (clojure.string/split (clojure.string/lower-case text) #" "))))
		
	; Now with a word-count-dict, evaluate what topic it fits.
	; getting data from wikipedia https://github.com/dakrone/clj-http
	; hopefully do this to mine topics? 
	; might also be done for getting dict.cc data. 
	; https://github.com/cgrand/enlive for html parsing.
	
))
