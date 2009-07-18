(ns com.mdelaurentis.engineyardcontest.webapp
  (:use [clojure.contrib test-is]
        [com.mdelaurentis.engineyardcontest solver]
        compojure)
  (:gen-class 
   :extends javax.servlet.http.HttpServlet))

(def manager (agent {}))

(defn link-bar []
  (interpose " | "
             [ (link-to "/problem"   "problem") 
               (link-to "/solutions" "solutions")]))

(defn html-doc [title & body]
  (html
   (doctype :html4)
   [:html
    [:head
     [:title title]
                                        ;(include-css "/styles.css")
     ]
    [:body
     (link-bar) body]
    ]))



(defroutes solver-app
  (GET "/"
    (html-doc 
     "Solver"))

  (GET "/problem"
    (html-doc 
     "Problem"
     [:h3 "Phrase:"] (:phrase @manager)
     [:h3 "Dictionary:"] (apply str (interpose " " (:dictionary @manager)))))
  
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

(defservice solver-app)

(defn -main [phrase-file dict-file num-solvers port cluster-file]
  (let [phrase  (slurp phrase-file)
        dict    (vec (.split (slurp dict-file) "\\s+"))
        solvers (map agent (make-solvers (Integer/valueOf num-solvers) phrase dict))]
    (send manager (fn [m] (make-solver "manager" phrase dict)))
    (println "solvers are" solvers)
    (doseq [solver solvers]
      (send solver solve (random-tweets dict)))
    (run-server {:port (Integer/valueOf port)} "/*" (servlet solver-app))
    (loop []
      (Thread/sleep 1000)
      (send manager accumulate (map deref solvers))
      (recur))))