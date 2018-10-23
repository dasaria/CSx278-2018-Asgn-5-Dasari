(ns asgnx.core
  (:require [clojure.string :as string]
            [clojure.core.async :as async :refer [go chan <! >!]]
            [asgnx.kvstore :as kvstore
             :refer [put! get! list! remove!]]))

;; map specifying munchie mart hours keyed by day of the week.
(def munchie-hours {
                    "rand"      {:monday    "8am-7:30pm"
                                 :tuesday   "8am-7:30pm"
                                 :wednesday "8am-7:30pm"
                                 :thursday  "8am-7:30pm"
                                 :friday    "8am-3pm"
                                 :saturday  "closed"
                                 :sunday    "closed"}

                    "commons"   {:monday    "24hours"
                                 :tuesday   "24hours"
                                 :wednesday "24hours"
                                 :thursday  "24hours"
                                 :friday    "24hours"
                                 :saturday  "24hours"
                                 :sunday    "24hours"}

                    "towers"    {:monday    "7am-3am"
                                 :tuesday   "7am-3am"
                                 :wednesday "7am-3am"
                                 :thursday  "7am-3am"
                                 :friday    "7am-11pm"
                                 :saturday  "9am-11pm"
                                 :sunday    "9am-3am"}

                    "branscomb" {:monday    "24hours"
                                 :tuesday   "24hours"
                                 :wednesday "24hours"
                                 :thursday  "24hours"
                                 :friday    "24hours"
                                 :saturday  "24hours"
                                 :sunday    "24hours"}

                    "kissam"    {:monday    "24hours"
                                 :tuesday   "24hours"
                                 :wednesday "24hours"
                                 :thursday  "24hours"
                                 :friday    "closes at 11pm"
                                 :saturday  "9am-11pm"
                                 :sunday    "opens at 9am"}

                    "highland"  {:monday    "7am-3am"
                                 :tuesday   "7am-3am"
                                 :wednesday "7am-3am"
                                 :thursday  "7am-3am"
                                 :friday    "7am-11pm"
                                 :saturday  "9am-11pm"
                                 :sunday    "9am-3am"}})

;; helper function that splits arguments
(defn words [msg]
  (if msg
      (string/split msg #" ")
      []))

;; returns the first word in a text message
(defn cmd [msg]
  (first (words msg)))

;; returns the list of words following the command in a text message.
(defn args [msg]
  (rest (words msg)))

;; returns a map with keys for the :cmd and :args parsed from the msg.
(defn parsed-msg [msg]
  {:cmd (cmd msg)
   :args (args msg)})

;;  takes a map in the format of days (as keys) and hours (as a string)
;; from the munchie-hours map and convert it to a printable format
(defn formatted-hours [location]
  (str "Sunday " (:sunday location) "\n"
       "Monday " (:monday location) "\n"
       "Tuesday " (:tuesday location) "\n"
       "Wednesday " (:wednesday location) "\n"
       "Thursday " (:thursday location) "\n"
       "Friday " (:friday location) "\n"
       "Saturday " (:saturday location)))

;; returns hours for a specific munchie mart. if requested munchie mart is
;; invalid or non-existant, returns "there is no munchie mart at that location"
(defn get-munchie-hours [{:keys [args]}]
  (let [newmsg (get munchie-hours (first args))]
    (if newmsg
      (str (first args) " hours:\n" (formatted-hours newmsg))
      "there is no munchie mart at that location")))

;; takes a destination for the msg in a parameter called `to`and the message in
;; a parameter called `msg` and returns a map with the keys :to and :msg bound
;; to each parameter.
(defn action-send-msg [to msg]
  {:to to
   :msg msg
   :action :send})

;; takes a list of people to receive a message in a `people`
;; parameter and a message to send them in a `msg` parmaeter
;; and returns a list produced by invoking the above `action-send-msg`
;; function on each person in the people list.
(defn action-send-msgs [people msg]
  (map #(action-send-msg % msg) people))

;; takes a list of keys in a `ks` parameter, a value to bind to that
;; key path to in a `v` parameter, and returns a map with
;; the key :ks bound to the `ks` parameter value and the key :v
;; vound to the `v` parameter value.)
(defn action-insert [ks v]
  {:action :assoc-in
   :ks ks
   :v v})

;; takes a list of keys in a `ks` parameter and returns a map with
;; the key :ks bound to the `ks` parameter value.
;; The map should also have the key :action bound to the value
;; :dissoc-in.
(defn action-remove [ks]
  {:action :dissoc-in
   :ks ks})

;; calls (action-insert combined-key value) for each possible
;; combined-key that can be produced by appending one of the suffixes
;; to the prefix.
(defn action-inserts [prefix ks v]
  (let [with-prefix (map #(conj prefix %) ks)]
    (map #(action-insert % v) with-prefix)))

;; calls (action-remove combined-key value) for each possible
;; combined-key that can be produced by appending one of the suffixes
;; to the prefix.
(defn action-removes [prefix ks]
  (let [with-prefix (map #(conj prefix %) ks)]
    (map #(action-remove %) with-prefix)))

;; adds employee to map under [:employee location]
(defn employee-register[employees location id info]
  [(action-insert [:employee location id] info)])

;; adds request to map under [:requests location]
(defn requests-register[requests location item]
  [(action-insert [:requests location item] {})])

(defn item-register[employees employee user-id q]
  [(action-insert [:conversations employee user-id q] {})])

;; removes employee from map from [:employee location]
(defn employee-unregister [employees location id]
  [(action-remove [:employee location id])])

;; creates message to return to user
(defn employees-question-msg [employees item-words]
  (str "Asking employee to search for "
       (string/join " " item-words)))

;; adds question to map under [:conversations ]
(defn add-question [employees {:keys [args user-id]}]
  (if (empty? employees)
    [[] "There is nobody working right now to help."]
    (if (empty? (rest args))
      [[] "You must ask to find something."]
      (let [q (clojure.string/join " " (rest args))
            emp (first employees)]
        [(concat (action-send-msgs employees q)
                 [(action-insert [:conversations emp user-id q] {})])
         (employees-question-msg employees (rest args))]))))

;; sends answer msg and removes question from conversations
(defn answer-question [conversation {:keys [user-id args]}]
  (if (empty? args)
    [[] "You did not provide an answer."]
    (if (empty? conversation)
      [[] "You haven't been asked a question."]
      (let [ans (clojure.string/join " " args)]
        [(concat [(action-send-msg (first conversation) ans)]
                 [(action-remove [:conversations user-id (first conversation)])])
         "Your answer was sent."]))))

;; removes all questions asked to user
(defn remove-questions [conversation {:keys [user-id]}]
  (if (empty? conversation)
    [[] "You don't have any questions."]
    [(concat (action-send-msgs conversation
                               "Your search request could not be completed. Please try again later.")
             [(action-remove [:conversations user-id conversation])])
     "Your questions have been cleared."]))

;; formats requests with commas
(defn format-requests [requests]
  (clojure.string/join ", " requests))

;; returns requests and removes requests
(defn get-requests [requests {:keys [args]}]
  (if (empty? requests)
    [[] "There are no requests."]
    (let [msg (format-requests requests)]
      [(action-removes [:requests (first args)] requests) msg])))

;; adds request to specific location
(defn add-request [requests {:keys [args]}]
  (if (empty? (rest args))
    [[] "You must request something."]
    (let [location (first args)
          item (clojure.string/join " " (rest args))
          msg (str "adding " item " to requests.")]
      [(requests-register requests location item) msg])))

;; adds employee to specific location
(defn checkin-employee [employees {:keys [args user-id]}]
  (let [location (first args)
        msg (str user-id " is now working at " location ".")]
    [(employee-register employees (first args) user-id {}) msg]))

;; removes employee from specific location
(defn checkout-employee [employees {:keys [args user-id]}]
  (let [location (first args)
        msg (str user-id " is now leaving " location ".")]
    [(employee-unregister employees (first args) user-id)
     msg]))

;; shows employee working at location
(defn get-employees [employees {:keys []}]
  (if (empty? employees)
    [[] "There are no employees working."]
    [[] (clojure.string/join ", " employees)]))

;; returns stateless output
(defn stateless [f]
  (fn [_ & args]
    [[] (apply f args)]))

;; mappings to 'routes' map where created
;; functions will be invoked when the corresponding text message
;; commands are received.
(def routes {"default"  (stateless (fn [& args] "Unknown command.")) ;; change to be help
             "getrequests" #(get-requests %1 %2)
             "checkin" #(checkin-employee %1 %2)
             "checkout" #(checkout-employee %1 %2)
             "answer" #(answer-question %1 %2)
             "find" #(add-question %1 %2)
             "clear" #(remove-questions %1 %2)
             "request" #(add-request %1 %2)
             "hours" (stateless get-munchie-hours)
             "workers" #(get-employees %1 %2)})

;; Queries******

;; returns state where requests are stored
(defn requests-for-food-query [state-mgr pmsg]
  (let [[location] (:args pmsg)]
    (list! state-mgr [:requests location])))

;; returns state with employees at a location
(defn employees-at-location-query [state-mgr pmsg]
  (let [[location] (:args pmsg)]
    (list! state-mgr [:employee location])))

;; returns state with conversations of specific user
(defn conversations-for-user-query [state-mgr pmsg]
  (let [user-id (:user-id pmsg)]
    (list! state-mgr [:conversations user-id])))


;; specifies needed states for different commands
(def queries
  {"getrequests" requests-for-food-query
   "checkin"  employees-at-location-query
   "checkout" employees-at-location-query
   "answer"   conversations-for-user-query
   "find"     employees-at-location-query
   "clear"   conversations-for-user-query
   "workers" employees-at-location-query
   "request"  requests-for-food-query})

;; reads state using specified query
(defn read-state [state-mgr pmsg]
  (go
    (if-let [qfn (get queries (:cmd pmsg))]
      (<! (qfn state-mgr pmsg))
      {})))

;; returns a function (<== pay attention to the return type that takes a
;; parsed message as input and returns the function in the `routes` map that
;; is associated with a key matching the `:cmd` in the parsed message.
(defn create-router [routes]
  #(let [newmsg (get routes (:cmd %))]
      (if newmsg
        newmsg
        (get routes "default"))))

;; gets output from o
(defn output [o]
  (second o))

;; gets first action from o actions
(defn actions [o]
  (first o))

;; invokes actions from user <see handle-message>
(defn invoke [{:keys [effect-handlers] :as system} e]
  (go
    (println "    Invoke:" e)
    (if-let [action (get effect-handlers (:action e))]
      (do
        (println "    Invoking:" action "with" e)
        (<! (action system e))))))

;; processes actions from user <see handle-message>
(defn process-actions [system actions]
  (go
    (println "  Processing actions:" actions)
    (let [results (atom [])]
      (doseq [action actions]
        (let [result (<! (invoke system action))]
          (swap! results conj result)))
      @results)))

;; Handles message from user
(defn handle-message
  "
    This function orchestrates the processing of incoming messages
    and glues all of the pieces of the processing pipeline together.

    The basic flow to handle a message is as follows:

    1. Create the router that will be used later to find the
       function to handle the message
    2. Parse the message
    3. Load any saved state that is going to be needed to process
       the message (e.g., querying the list of experts, etc.)
    4. Find the function that can handle the message
    5. Call the handler function with the state from #3 and
       the message
    6. Run the different actions that the handler returned...these actions
       will be bound to different implementations depending on the environemnt
       (e.g., in test, the actions aren't going to send real text messages)
    7. Return the string response to the message

  "
  [{:keys [state-mgr] :as system} src msg]
  (go
    (println "=========================================")
    (println "  Processing:\"" msg "\" from" src)
    (let [rtr    (create-router routes)
          _      (println "  Router:" rtr)
          pmsg   (assoc (parsed-msg msg) :user-id src)
          _      (println "  Parsed msg:" pmsg)
          state  (<! (read-state state-mgr pmsg))
          _      (println "  Read state:" state)
          hdlr   (rtr pmsg)
          _      (println "  Hdlr:" hdlr)
          [as o] (hdlr state pmsg)
          _      (println "  Hdlr result:" [as o])
          arslt  (<! (process-actions system as))
          _      (println "  Action results:" arslt)]
      (println "=========================================")
      o)))
