(ns asgnx.core-test
  (:require [clojure.test :refer :all]
            [clojure.core.async :refer [<!!]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test.check.generators :as gen]
            [asgnx.core :refer :all]
            [asgnx.kvstore :as kvstore :refer [put! get!]]))



(deftest words-test
  (testing "that sentences can be split into their constituent words"
    (is (= ["a" "b" "c"] (words "a b c")))
    (is (= [] (words "   ")))
    (is (= [] (words nil)))
    (is (= ["a"] (words "a")))
    (is (= ["a"] (words "a ")))
    (is (= ["a" "b"] (words "a b")))))


(deftest cmd-test
  (testing "that commands can be parsed from text messages"
    (is (= "foo" (cmd "foo")))
    (is (= "foo" (cmd "foo x y")))
    (is (= nil   (cmd nil)))
    (is (= ""    (cmd "")))))


(deftest args-test
  (testing "that arguments can be parsed from text messages"
    (is (= ["x" "y"] (args "foo x y")))
    (is (= ["x"] (args "foo x")))
    (is (= [] (args "foo")))
    (is (= [] (args nil)))))


(deftest parsed-msg-test
  (testing "that text messages can be parsed into cmd/args data structures"
    (is (= {:cmd "foo"
            :args ["x" "y"]}
           (parsed-msg "foo x y")))
    (is (= {:cmd "foo"
            :args ["x"]}
           (parsed-msg "foo x")))
    (is (= {:cmd "foo"
            :args []}
           (parsed-msg "foo")))
    (is (= {:cmd "foo"
            :args ["x" "y" "z" "somereallylongthing"]}
           (parsed-msg "foo x y z somereallylongthing")))))

(deftest formatted-hours-test
  (testing "that the office hours data structure is correctly converted to a string"
    (is (= (str "Sunday sunhours\n"
                "Monday monhours\n"
                "Tuesday tueshours\n"
                "Wednesday wedhours\n"
                "Thursday thurshours\n"
                "Friday frihours\n"
                "Saturday sathours")
           (formatted-hours {:saturday "sathours"
                             :sunday "sunhours"
                             :monday "monhours"
                             :tuesday "tueshours"
                             :wednesday "wedhours"
                             :thursday "thurshours"
                             :friday "frihours"})))))

(deftest munchie-hours-test
  (testing "testing lookup of office hours on a specific day"
    (is (= (str "branscomb hours:\n"
                "Sunday 24hours\n"
                "Monday 24hours\n"
                "Tuesday 24hours\n"
                "Wednesday 24hours\n"
                "Thursday 24hours\n"
                "Friday 24hours\n"
                "Saturday 24hours")
           (get-munchie-hours {:cmd "hours" :args ["branscomb"]})))
    (is (= (str "rand hours:\n"
                "Sunday closed\n"
                "Monday 8am-7:30pm\n"
                "Tuesday 8am-7:30pm\n"
                "Wednesday 8am-7:30pm\n"
                "Thursday 8am-7:30pm\n"
                "Friday 8am-3pm\n"
                "Saturday closed")
           (get-munchie-hours {:cmd "hours" :args ["rand"]})))))


(deftest create-router-test
  (testing "correct creation of a function to lookup a handler for a parsed message"
    (let [router (create-router {"hello" #(str (:cmd %) " " "test")
                                 "argc"  #(count (:args %))
                                 "echo"  identity
                                 "default" (fn [& a] "No!")})
          msg1   {:cmd "hello"}
          msg2   {:cmd "argc" :args [1 2 3]}
          msg3   {:cmd "echo" :args ["a" "z"]}
          msg4   {:cmd "echo2" :args ["a" "z"]}]
      (is (= "hello test" ((router msg1) msg1)))
      (is (= "No!" ((router msg4) msg4)))
      (is (= 3 ((router msg2) msg2)))
      (is (= msg3 ((router msg3) msg3))))))


(deftest action-send-msg-test
  (testing "That action send msg returns a correctly formatted map"
    (is (= :send
           (:action (action-send-msg :bob "foo"))))
    (is (= :bob
           (:to (action-send-msg :bob "foo"))))
    (is (= "foo"
           (:msg (action-send-msg [:a :b] "foo"))))))


(deftest action-send-msgs-test
  (testing "That action send msgs generates a list of sends"
    (let [a (action-send-msg [:a :f :b] 1)
          b (action-send-msg [:a :f :d] 1)
          c (action-send-msg [:a :f :e] 1)
          d (action-send-msg [:a :f :c] 1)]
      (is (= [a b c d]
             (action-send-msgs [[:a :f :b]
                                [:a :f :d]
                                [:a :f :e]
                                [:a :f :c]]
                              1))))))

(deftest action-insert-test
  (testing "That action insert returns a correctly formatted map"
    (is (= #{:action :ks :v}
           (into #{}(keys (action-insert [:a :b] {:foo 1})))))
    (is (= #{:assoc-in [:a :b] {:foo 1}}
           (into #{}(vals (action-insert [:a :b] {:foo 1})))))
    (is (= :assoc-in
           (:action (action-insert [:a :b] {:foo 1}))))
    (is (= {:foo 1}
           (:v (action-insert [:a :b] {:foo 1}))))
    (is (= [:a :b]
           (:ks (action-insert [:a :b] {:foo 1}))))))


(deftest action-remove-test
  (testing "That action remove returns a correctly formatted map"
    (is (= #{:action :ks}
         (into #{} (keys (action-remove [:a :b])))))
    (is (= #{:dissoc-in [:a :b]}
          (into #{}(vals (action-remove [:a :b])))))
    (is (= :dissoc-in
           (:action (action-remove [:a :b]))))
    (is (= [:a :b]
           (:ks (action-remove [:a :b]))))))


(deftest action-inserts-test
  (testing "That action inserts generates a list of inserts"
    (let [a (action-insert [:a :f :b] 1)
          b (action-insert [:a :f :d] 1)
          c (action-insert [:a :f :e] 1)
          d (action-insert [:a :f :c] 1)]
      (is (= [a b c d]
             (action-inserts [:a :f] [:b :d :e :c] 1))))))


(defn action-send [system {:keys [to msg]}]
  (put! (:state-mgr system) [:msgs to] msg))

(defn pending-send-msgs [system to]
  (get! (:state-mgr system) [:msgs to]))

(def send-action-handlers
  {:send action-send})

(deftest handle-message-test
  (testing "the integration and handling of messages"
    (let [ehdlrs (merge
                   send-action-handlers
                   kvstore/action-handlers)
          state  (atom {})
          smgr   (kvstore/create state)
          system {:state-mgr smgr
                  :effect-handlers ehdlrs}]
      ;; requests test - no requests
      (is (= "There are no requests."
             (<!! (handle-message
                    system
                    "test-user"
                    "requests branscomb"))))
      ;; request test - nothing requested
      (is (= "You must request something."
             (<!! (handle-message
                    system
                    "test-user"
                    "request branscomb"))))
      ;; request test- request something
      (is (= "adding sunshine to requests."
             (<!! (handle-message
                    system
                    "test-user"
                    "request branscomb sunshine"))))
      ;; request test- request something
      (is (= "adding lollipops to requests."
             (<!! (handle-message
                    system
                    "test-user"
                    "request branscomb lollipops"))))
      ;; request test- request something
      (is (= "adding and rainbows to requests."
             (<!! (handle-message
                    system
                    "test-user"
                    "request branscomb and rainbows"))))
      ;; requests test - get requests multiple
      (is (= "sunshine, lollipops, and rainbows"
             (<!! (handle-message
                    system
                    "test-admin"
                    "requests branscomb"))))
      ;; find test - when no employees
      (is (= "There is nobody working right now to help."
             (<!! (handle-message
                   system
                   "test-user"
                   "find branscomb chex mix"))))
      ;; checkin test - when noone working
      (is (= "test-employee is now working at highland."
             (<!! (handle-message
                   system
                   "test-employee"
                   "checkin highland"))))
      ;; checkin test - when someone work somewhere else
      (is (= "test-employee2 is now working at rand."
             (<!! (handle-message
                   system
                   "test-employee2"
                   "checkin rand"))))
      ;; find test - when nothing asked for
      (is (= "You must ask to find something."
             (<!! (handle-message
                   system
                   "test-user"
                   "find highland"))))
      ;; checkout test - when someone working
      (is (= "test-employee is now leaving highland."
             (<!! (handle-message
                   system
                   "test-employee"
                   "checkout highland"))))
      ;; checkout test - checks if worked
      (is (= "There is nobody working right now to help."
             (<!! (handle-message
                   system
                   "test-user"
                   "find highland something good"))))
      ;; answer test no questions
      (is (= "You haven't been asked a question."
             (<!! (handle-message
                   system
                   "test-employee2"
                   "answer okeydokes"))))
      ;; find test
      (is (= "Asking employee to search for something good"
             (<!! (handle-message
                   system
                   "test-user"
                   "find rand something good"))))
      (is (= "something good"
             (<!! (pending-send-msgs system "test-employee2"))))
      (is (= "Asking employee to search for something new"
             (<!! (handle-message
                   system
                   "test-user2"
                   "find rand something new"))))
      (is (= "something new"
             (<!! (pending-send-msgs system "test-employee2"))))
      ;; answer test no questions
      (is (= "You did not provide an answer."
             (<!! (handle-message
                   system
                   "test-employee2"
                   "answer"))))
      (is (= "Your answer was sent."
             (<!! (handle-message
                   system
                   "test-employee2"
                   "answer YES"))))
      (is (= "YES"
             (<!! (pending-send-msgs system "test-user"))))
      (is (= "Your answer was sent."
             (<!! (handle-message
                   system
                   "test-employee2"
                   "answer NO"))))
      (is (= "NO"
             (<!! (pending-send-msgs system "test-user2")))))))
