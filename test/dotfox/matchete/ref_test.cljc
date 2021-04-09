(ns dotfox.matchete.ref-test
  (:require [dotfox.matchete :as m]
            #?(:clj [clojure.test :as t :refer [deftest is]]
               :cljs [cljs.test :as t :refer [deftest is] :include-macros true])))

(deftest ref-test
  (is (= [{:path [:a], :value 42}
          {:value
           {:a 42, :b {:c 42, :d {:e 42}}, :f [42 {:g 42} [{:h {:i 42}} 42]]}}
          {:path [:b :c], :value 42}
          {:path [:f 0], :value 42}
          {:path [:b], :value {:c 42, :d {:e 42}}}
          {:path [:f], :value [42 {:g 42} [{:h {:i 42}} 42]]}
          {:path [:b :d :e], :value 42}
          {:path [:f 1 :g], :value 42}
          {:path [:b :d], :value {:e 42}}
          {:path [:f 2 0 :h :i], :value 42}
          {:path [:f 1], :value {:g 42}}
          {:path [:f 2], :value [{:h {:i 42}} 42]}
          {:path [:f 2 1], :value 42}
          {:path [:f 2 0], :value {:h {:i 42}}}
          {:path [:f 2 0 :h], :value {:i 42}}]
         ((m/matcher
           {:walk [:or
                   [:search [:mvar :path] [:ref :walk]]
                   [:lvar :value]]}
           [:ref :walk])
          {:a 42
           :b {:c 42
               :d {:e 42}}
           :f [42 {:g 42} [{:h {:i 42}} 42]]}))))

(deftest sample
  (let [data {:deps {'acme/spaceship {:mvn/version "1.0.0"}
                     'org.clojure/clojure {:mvn/version "1.10.1"}}
              :aliases {:moon {:extra-deps {'moon/lander {:mvn/version "0.8.7"}
                                            'rock/examiner {:mvn/version "4.0.21"}}
                               :override-deps {'foo/bar {:mvn/version "1.2.3"}}}
                        :mars {:extra-deps {'mars/rover {:mvn/version "1.14.9"}
                                            'comms/serial {:mvn/version "2.4.2"}}
                               :default-deps {'hello/world {:mvn/version "9.8.7"}}}}}
        matcher (m/matcher {:walk [:or
                                   [:and
                                    [:map-of symbol? [:map [:mvn/version string?]]]
                                    [:lvar :val]]
                                   [:search [:mvar :path] [:ref :walk]]]}
                           [:ref :walk])]
    (is (= [{:path [:deps],
             :val {'acme/spaceship #:mvn{:version "1.0.0"},
                   'org.clojure/clojure #:mvn{:version "1.10.1"}}}
            {:path [:aliases :moon :extra-deps],
             :val {'moon/lander #:mvn{:version "0.8.7"},
                   'rock/examiner #:mvn{:version "4.0.21"}}}
            {:path [:aliases :mars :extra-deps],
             :val {'mars/rover #:mvn{:version "1.14.9"},
                   'comms/serial #:mvn{:version "2.4.2"}}}
            {:path [:aliases :moon :override-deps],
             :val {'foo/bar #:mvn{:version "1.2.3"}}}
            {:path [:aliases :mars :default-deps],
             :val {'hello/world #:mvn{:version "9.8.7"}}}]
           (matcher data)))))
