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
(defn get-munchie-hours [{:keys [args cmd]}]
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

;; Asgn 2.
;;
;; @Todo: Create a function called action-send-msgs that takes
;; takes a list of people to receive a message in a `people`
;; parameter and a message to send them in a `msg` parmaeter
;; and returns a list produced by invoking the above `action-send-msg`
;; function on each person in the people list.
;;
;; java-like pseudo code:
;;
;; output = new list
;; for person in people:
;;   output.add( action-send-msg(person, msg) )
;; return output
;;
(defn action-send-msgs [people msg]
  (map #(action-send-msg % msg) people))

;; Asgn 2.
;;
;; @Todo: Create a function called action-insert that takes
;; a list of keys in a `ks` parameter, a value to bind to that
;; key path to in a `v` parameter, and returns a map with
;; the key :ks bound to the `ks` parameter value and the key :v
;; vound to the `v` parameter value.)
;; The map should also have the key :action bound to the value
;; :assoc-in.
;;
(defn action-insert [ks v]
  {:action :assoc-in
   :ks ks
   :v v})

;; Asgn 2.
;;
;; @Todo: Create a function called action-inserts that takes:
;; 1. a key prefix (e.g., [:a :b])
;; 2. a list of suffixes for the key (e.g., [:c :d])
;; 3. a value to bind
;;
;; and calls (action-insert combined-key value) for each possible
;; combined-key that can be produced by appending one of the suffixes
;; to the prefix.
;;
;; In other words, this invocation:
;;
;; (action-inserts [:foo :bar] [:a :b :c] 32)
;;
;; would be equivalent to this:
;;
;; [(action-insert [:foo :bar :a] 32)
;;  (action-insert [:foo :bar :b] 32)
;;  (action-insert [:foo :bar :c] 32)]
;;
(defn action-inserts [prefix ks v]
  (let [with-prefix (map #(conj prefix %) ks)]
    (map #(action-insert % v) with-prefix)))

;; Asgn 2.
;;
;; @Todo: Create a function called action-remove that takes
;; a list of keys in a `ks` parameter and returns a map with
;; the key :ks bound to the `ks` parameter value.
;; The map should also have the key :action bound to the value
;; :dissoc-in.
;;
(defn action-remove [ks]
  {:action :dissoc-in
   :ks ks})

(defn employee-register[employees location id info]
  [(action-insert [:employee location id] info)])

(defn requests-register[requests location item]
  [(action-insert [:requests location item] {})])

(defn employee-unregister [employees location id]
  [(action-remove [:employee location id])])

(defn requests-remove [requests location]
  [(action-remove [:requests location])])

(defn employees-question-msg [employees item-words]
  (str "Asking employee to search for "
       (string/join " " item-words)))

;; Asgn 3.
;;
;; @Todo: Create a function called "ask-experts"
;; that takes two parameters:
;;
;; 1. the list of experts on the topic
;; 2. a parsed message with the format:
;;    {:cmd "ask"
;;     :user-id "phone number that sent the message"
;;     :args [topic question-word1 question-word2 ... question-wordN]}
;;
;; The sender of the message will be identified by their phone number
;; in the user-id parameter. This is the phone number that you will need
;; to forward answers to the question to.
;;
;; The parsed message is generated by breaking up the words in the ask
;; text message. For example, if someone sent the message:
;;
;; "ask food what is the best pizza in nashville"
;;
;; The parsed message would be:
;;
;; {:cmd "ask"
;;  :user-id "+15555555555"
;;  :args ["food" "what" "is" "the" "best" "pizza" "in" "nashville"]}
;;
;; This function needs to return a list with two elements:
;; [[actions...] "response to asker"]
;;
;; The actions in the list are the *side effects* that need to take place
;; to ask the question (e.g., sending messages to the experts). The string
;; is the response that is going to be sent back to the person that asked
;; the question (e.g. "Asking 2 expert(s) for an answer to ....").
;;
;; The correct string response to a valid question should be produced with
;; the `experts-question-msg` function above.
;;
;; Think about how you are going to figure out where to route messages
;; when an expert answers (see the conversations query) and make sure you
;; handle the needed side effect for storing the conversation state.
;;
;; If there are no registered experts on a topic, you should return an
;; empty list of actions and "There are no experts on that topic."
;;
;; If there isn't a question, you should return "You must ask a valid question."
;;
;; Why this strange architecture? By returning a list of the actions to take,
;; rather than directly taking that action, we can keep this function pure.
;; Pure functions are WAY easier to test / maintain. Also, we can isolate our
;; messy impure action handling at the "edges" of the application, where it is
;; easier to track and reason about.
;;
;; You should look at `handle-message` to get an idea of the way that this
;; function is going to be used, its expected signature, and how the actions
;; and output are going to work.
;;
;; See the integration test in See handle-message-test for the
;; expectations on how your code operates
;;
(defn ask-experts [experts {:keys [args user-id]}]
  (if (nil? experts)
    [[] "There are no experts on that topic."]
    (if (empty? (rest args))
      [[] "You must ask a valid question."]
      (let [q (clojure.string/join " " (rest args))]
        [(concat (action-send-msgs experts q)
                 (action-inserts [:conversations] experts {:last-question q :asker user-id}))
         (employees-question-msg experts (rest args))]))))


(defn add-question [employees {:keys [args user-id]}]
  (if (nil? employees)
    [[] "There is nobody working right now to help."]
    (if (empty? (rest args))
      [[] "You must ask to find something."]
      (let [q (clojure.string/join " " (rest args))]
        [(concat (action-send-msgs employees q)
                 (action-inserts [:conversations (first employees) user-id] [:last-question] q))
         (employees-question-msg employees (rest args))]))))

;; Asgn 3.
;;
;; @Todo: Create a function called "answer-question"
;; that takes two parameters:
;;
;; 1. the last conversation describing the last question that was routed
;;    to the expert
;; 2. a parsed message with the format:
;;    {:cmd "ask"
;;     :user-id "+15555555555"
;;     :args [topic answer-word1 answer-word2 ... answer-wordN]}
;;
;; The parsed message is generated by breaking up the words in the ask
;; text message. For example, if someone sent the message:
;;
;; "answer joey's house of pizza"
;;
;; The conversation will be data that you store as a side-effect in
;; ask-experts. You probably want this data to be information about the
;; last question asked to each expert. See the "think about" comment above.
;;
;; The parsed message would be:
;;
;; {:cmd "answer"
;;  :user-id "+15555555555"
;;  :args ["joey's" "house" "of" "pizza"]}
;;
;; This function needs to return a list with two elements:
;; [[actions...] "response to expert answering"]
;;
;; The actions in the list are the *side effects* that need to take place
;; to send the answer to the original question asker. The string
;; is the response that is going to be sent back to the expert answering
;; the question.
;;
;; Think about how you are going to figure out where to route messages
;; when an expert answers (see the conversations query) and make sure you
;; handle the needed side effect for storing the conversation state.
;;
;; Why this strange architecture? By returning a list of the actions to take,
;; rather than directly taking that action, we can keep this function pure.
;; Pure functions are WAY easier to test / maintain. Also, we can isolate our
;; messy impure action handling at the "edges" of the application, where it is
;; easier to track and reason about.
;;
;; You should look at `handle-message` to get an idea of the way that this
;; function is going to be used, its expected signature, and how the actions
;; and output are going to work.
;;
;; See the integration test in See handle-message-test for the
;; expectations on how your code operates
;;
(defn answer-question [conversation {:keys [user-id args]}]
  (if (empty? args)
    [[] "You did not provide an answer."]
    (if (empty? conversation)
      [[] "You haven't been asked a question."]
      (let [ans (clojure.string/join " " args)]
        [(concat [(action-send-msg (first (first conversation)) ans)]
                 [(action-remove [:conversations user-id (first (first conversation))])])
         "Your answer was sent."]))))

(defn format-requests [requests]
  (clojure.string/join ", " requests))

(defn get-requests [requests {:keys [location]}]
  (if (empty? requests)
    [[] "There are no requests."]
    (let [msg (format-requests requests)]
      [(requests-remove requests location) msg])))

(defn add-request [requests {:keys [args]}]
  (if (empty? (rest args))
    [[] "You must request something."]
    (let [location (first args)
          item (clojure.string/join " " (rest args))
          msg (str "adding " item " to requests.")]
      [(requests-register requests location item) msg])))

(defn checkin-employee [employees {:keys [args user-id]}]
  (let [location (first args)
        msg (str user-id " is now working at " location ".")]
    [(employee-register employees (first args) user-id {}) msg]))

(defn checkout-employee [employees {:keys [args user-id]}]
  (let [location (first args)
        msg (str user-id " is now leaving " location ".")]
    [(employee-unregister employees (first args) user-id) msg]))

;; Don't edit!
(defn stateless [f]
  (fn [_ & args]
    [[] (apply f args)]))

(def routes {"default"  (stateless (fn [& args] "Unknown command.")) ;; change to be help
             "requests" #(get-requests %1 %2)
             "checkin" #(checkin-employee %1 %2)
             "checkout" #(checkout-employee %1 %2)
             ;; "question" #(send-question)
             "answer" #(answer-question %1 %2)
             "find" #(add-question %1 %2)
             ;; "cancel" #(remove-question)
             "request" #(add-request %1 %2)
             "hours" (stateless get-munchie-hours)})


;; Asgn 3.
;;
;; @Todo: Add mappings of the cmds "expert", "ask", and "answer" to
;; to the `routes` map so that the functions that you
;; created will be invoked when the corresponding text message
;; commands are received.
;;})


;; ****************************************************************************
(defn requests-for-food-query [state-mgr pmsg]
  (let [[location] (:args pmsg)]
    (list! state-mgr [:requests location])))

(defn employees-at-location-query [state-mgr pmsg]
  (let [[location] (:args pmsg)]
    (list! state-mgr [:employee location])))

(defn conversations-for-user-query [state-mgr pmsg]
  (let [user-id (:user-id pmsg)]
    (get! state-mgr [:conversations user-id])))

(def queries
  {"requests" requests-for-food-query
   "checkin"  employees-at-location-query
   "checkout" employees-at-location-query
   "question" conversations-for-user-query
   "answer"   conversations-for-user-query
   "find"     employees-at-location-query
   "cancel"   conversations-for-user-query
   "request"  requests-for-food-query})

;; Don't edit!
(defn read-state [state-mgr pmsg]
  (go
    (if-let [qfn (get queries (:cmd pmsg))]
      (<! (qfn state-mgr pmsg))
      {})))

;; Asgn 1.
;;
;; @Todo: This function should return a function (<== pay attention to the
;; return type) that takes a parsed message as input and returns the
;; function in the `routes` map that is associated with a key matching
;; the `:cmd` in the parsed message. The returned function would return
;; `welcome` if invoked with `{:cmd "welcome"}`.
;;
;; Example:
;;
;; (let [msg {:cmd "welcome" :args ["bob"]}]
;;   (((create-router {"welcome" welcome}) msg) msg) => "Welcome bob"
;;
;; If there isn't a function in the routes map that is mapped to a
;; corresponding key for the command, you should return the function
;; mapped to the key "default".
;;
;; See the create-router-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn create-router [routes]
  #(let [newmsg (get routes (:cmd %))]
      (if newmsg
        newmsg
        (get routes "default"))))

;; Don't edit!
(defn output [o]
  (second o))

;; Don't edit!
(defn actions [o]
  (first o))

;; Don't edit!
(defn invoke [{:keys [effect-handlers] :as system} e]
  (go
    (println "    Invoke:" e)
    (if-let [action (get effect-handlers (:action e))]
      (do
        (println "    Invoking:" action "with" e)
        (<! (action system e))))))

;; Don't edit!
(defn process-actions [system actions]
  (go
    (println "  Processing actions:" actions)
    (let [results (atom [])]
      (doseq [action actions]
        (let [result (<! (invoke system action))]
          (swap! results conj result)))
      @results)))

;; Don't edit!
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
