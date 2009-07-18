(ns com.mdelaurentis.engineyardcontest.webapp
  (:import [java.net InetAddress])
  (:use [clojure.contrib test-is duck-streams]
        [com.mdelaurentis.engineyardcontest solver]
        compojure)
  (:gen-class 
   :extends javax.servlet.http.HttpServlet))

(def hostname (.getHostName (InetAddress/getLocalHost)))

(def manager (agent {}))

(def cluster (ref []))


(defn link-bar []
  (interpose " | "
             [ (link-to "/problem"   "problem") 
               (link-to "/solutions" "solutions")
               (link-to "/cluster" "cluster")]))

(defn html-doc [title & body]
  (html
   (doctype :html4)
   [:html
    [:head
     [:title title]
     ;;(include-css "/styles.css")
     ]
    [:body
     (link-bar) body]
    ]))



(defroutes solver-app
  (GET "/"
    (redirect-to "/solutions"))

  (GET "/problem"
    (html-doc 
     "Problem"
     [:h3 "Phrase:"] (:phrase @manager)
     [:h3 "Dictionary:"] (apply str (interpose " " (:dictionary @manager)))))
  
  (GET "/manager"
    (str (select-keys @manager [:id :solutions :num-tries])))

  (GET "/cluster"
    (html-doc
     "Cluster"
     [:table 
      [:tr (for [header ["Host"]]
             [:td header])]
      (for [host @cluster]
        [:tr
         [:td [:a {:href (str "http://" host)} host]]])]))

  (GET "/solutions"
    (html-doc "Solutions"
     [:h3 "Num Tried:"] (:num-tries @manager)
     [:h3 "Solutions:"]
     [:table
      [:tr 
       [:td "Phrase"]
       [:td "Score"]
       [:td "Time Found"]]
      (for [s (:solutions @manager)]
        [:tr
         [:td (:tweet s)]
         [:td (:score s)]
         [:td (:time s)]])])))

(defn -main [phrase-file dict-file num-solvers port cluster-file]
  (let [phrase  (slurp phrase-file)
        dict    (vec (.split (slurp dict-file) "\\s+"))
        hosts   (vec (.split (slurp cluster-file) "\\s+"))
        solvers (map agent (make-solvers (Integer/valueOf num-solvers) phrase dict))]
    (dosync (ref-set cluster hosts))
    (send manager (fn [m] (make-solver "manager" phrase dict)))
    (doseq [solver solvers]
      (send solver solve (random-tweets dict)))
    (run-server {:port (Integer/valueOf port)} "/*" (servlet solver-app))
    (loop [n 0]
      (Thread/sleep 1000)
      (send manager accumulate (map deref solvers))
      (if (zero? (mod n 10))
        (send manager accumulate))
      (recur (mod (inc n) 10)))))