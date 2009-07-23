(ns com.mdelaurentis.engineyardcontest.webapp
  (:import [java.net InetAddress URL]
           [java.util Date])
  (:use [clojure.contrib test-is duck-streams]
        [com.mdelaurentis.engineyardcontest solver]
        compojure)
  (:gen-class 
   :extends javax.servlet.http.HttpServlet))

(set! *warn-on-reflection* true)

;; Number of milliseconds for to sleep between 
;; rounds of polling local solvers (ten seconds)
(def local-update-time-millis 10000)

;; Number of local updates to perform
;; between remote updates (once a minute)
(def remote-update-frequency 6)

(def hostname (.getHostName (InetAddress/getLocalHost)))

;; Responsible for accumulating results
;; from local and remote solvers
(def manager (agent {}))

;; Ref of vector of agents, each one representing
;; the state of a remote solver
(def cluster (ref []))

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
     ;;(include-css "/styles.css")
     ]
    [:body
     (link-bar) body]
    ]))

(defn solution-table [solutions]
  [:table
   [:tr 
    [:th "Phrase"]
    [:th "Score"]
    [:th "Time Found"]]
   (for [s solutions]
     [:tr
      [:td (:tweet s)]
      [:td (:score s)]
      [:td (Date. (:time s))]])])

(defn cluster-table [hosts]
  [:table 
   [:tr (for [header ["Host" "Num Tries" "Best Score" "Last Update" "Error"]]
          [:th header])]
   (for [host hosts]
     [:tr
      [:td [:a {:href (str "http://" (:id host) "/solutions")} (:id host)]]
      [:td (:num-tries host)]
      [:td (best-score host)]
      [:td (Date. (:last-update host))]
      [:td (when (:error host)
             (.getMessage (:error host)))]])
   [:tr
    [:td [:a {:href "/solutions"}] "total"]
    [:td (apply + (map :num-tries hosts))]
    [:td (first (sort (map best-score hosts)))]
    [:td ""]
    [:td ""]]])

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

  (GET "/solutions"
    (html-doc
     "Cluster" 
     
     [:h2 "Best Solution:"]
     (solution-table 
      (list
       (best-solution
        (accumulate
         (solver "cluster" (:phrase @manager) (:dictionary @manager))
         (map deref @cluster)))))

     [:h2 "Cluster:"]
     (cluster-table (map deref @cluster))
          
     [:h2 "Solutions on " hostname ":"]
     (solution-table (:solutions @manager)))))

(defn poll-remote-solver 
  "Attempts to update solver with the :solutions and :num-tries from
  the remote solver.  If an error is caught, sets :error to the
  Throwable object.  Sets :last-update to the current time."
  [slvr]
  (assoc
      (try 
       (let [status (read-string (slurp* (:url slvr)))]
         (dissoc (merge slvr (select-keys status [:solutions :num-tries]))
                 :error))
       (catch Throwable t (assoc slvr :error t)))
    :last-update (System/currentTimeMillis)))

(defn remote-solver [host phrase dict]
  (assoc (solver host phrase dict)
    :url (URL. (str "http://" host "/manager"))))

(defn -main [#^String phrase-file 
             #^String dict-file
             #^String num-solvers
             #^String port
             #^String cluster-file]
  (let [phrase  (slurp phrase-file)
        dict    (vec (.split (slurp dict-file) "\\s+"))
        hosts   (vec (.split (slurp cluster-file) "\\s+"))
        slvrs (map agent (solvers (Integer/valueOf num-solvers) phrase dict))]
    (dosync 
     (ref-set cluster 
              (for [host hosts]
                (agent (remote-solver host phrase dict)))))
    (send manager (fn [m] (solver "manager" phrase dict)))
    (doseq [slvr slvrs]
      (send slvr solve (random-tweets dict)))
    (run-server {:port (Integer/valueOf port)} "/*" (servlet solver-app))
    (loop [n 0]
      (Thread/sleep local-update-time-millis)
      (send manager accumulate (map deref slvrs))
      (when (zero? (mod n remote-update-frequency))
        (do
          (doseq [remote @cluster]
            (send-off remote poll-remote-solver))))
      (recur (mod (inc n) remote-update-frequency)))))