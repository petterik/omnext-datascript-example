(ns petterik.om_next_example.core
  (:require [goog.dom :as gdom]
            [sablono.core :as html :refer-macros [html]]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [datascript.core :as d]))

;; First define some schema and data.
(def schema {:person/likes {:db/valueType :db.type/ref
                            :db/cardinality :db.cardinality/many}
             :db/id        {:db/unique :db.unique/identity}
             :person/name  {:db/unique :db.unique/identity}
             :interest     {:db/unique :db.unique/identity}
             :app          {:db/unique :db.unique/identity}})

(def init-data [{:db/id -1 :interest :clojurescript}
                {:db/id -2 :interest :music}
                {:person/name "Petter" :person/twitter "@ipetterik_"  :person/likes [-1 -2]}
                {:person/name "David"  :person/twitter "@swannodette" :person/likes [-1 -2]}
                {:app :state :app/interest :clojurescript}])

;; mutate and read functions for om.next's parser
(defmulti mutate om/dispatch)
(defmulti read om/dispatch)

(defmethod read :default
  [{:keys [state selector] :as env} key params]
  ;; TODO: Figure out why this is called sometimes on my Person's IQuery.
  ;; We don't have enough context to do anything, so we're just returning nil.
  ;; Is this the right thing to do? Why is it called?
  )


(defmethod read :app/list-interests 
  "list all intersts and use the selector to pull the data the components need.
  The selector is defined by each component's IQuery"
  [{:keys [state selector]} _ _] 
  {:value (d/q '[:find [(pull ?interest ?selector) ...]
                 :in $ ?selector
                 :where [?interest :interest]]
               (d/db state)
               selector)})

(defmethod mutate 'person/dislike 
  "Dislike an interest given an entity"
  [{:keys [state]} _ {:keys [entity interest] :as p}]
  {:action #(d/transact! state [[:db/retract (:db/id entity) 
                                 :person/likes [:interest interest]]])})

(defmethod mutate 'person/make-bold 
  "Make something bold. Note: update-in takes an entity map, not an entity id for some reason."
  [{:keys [state]} _ {:keys [entity]}]
  {:action #(d/transact! state [(update-in entity [:ui.person/bold] not)])})

(defn button [this text transaction]
  [:button {:on-click #(om/transact! this transaction)} text])

(declare RootView)

;; Person component, defining what data it needs from a person
(defui Person
  static om/IQuery
  (query [this]
         [:db/id :ui.person/bold
          :person/name :person/twitter {:person/likes [:interest]}])
  Object
  (render [this]
          (let [{:keys [person/name
                        person/twitter
                        person/likes
                        ui.person/bold
                        like] :as entity} (om/props this)
                root-query (first (om/get-query RootView))]
            (prn "Rendering person: " {:name name :who-like like})
            (html [:div 
                   [:p 
                    {:style #js {:fontWeight (if bold "bold" "normal")}} 
                    (str twitter)]
                   (button this "dislike"
                           [`(person/dislike {:entity ~entity :interest ~like}) root-query])
                   (button this (if bold "normal" "bold")
                                  [`(person/make-bold {:entity ~entity}) 
                                   ;; Note: We cannot just re-read the Person's IQuery. It
                                   ;; needs to be used in a pull syntax. We need to re-read
                                   ;; the :app/list-interests key with all of it's dependencies.
                                   ;; Om will do a good job of just rendering the entities
                                   ;; which need re-rendering. Same thing goes for the dislike
                                   ;; button.
                                   root-query])]))))

(def person (om/factory Person {:keyfn :person/name}))

(defui InterestedPeople
  static om/IQuery
  (query [this]
         [:db/id :interest {:person/_likes (om/get-query Person)}])
  Object
  (render [this]
          (let [{:keys [db/id interest person/_likes]} (om/props this)]
            (prn "Rendering People interested in: " (name interest))
            (html
              [:div [:h2 (str "People who like " (name interest) ":")]
               (when _likes
                 ;; We can pass data to the person from it's parent.
                 [:div (map #(person (assoc % :like interest)) _likes)])]))))

(def interested-people (om/factory InterestedPeople {:keyfn :interest}))

(defui RootView
  static om/IQuery
  (query [this] [{:app/list-interests (om/get-query InterestedPeople)}])
  Object
  (render [this]
          (prn "Rendering RootView")
          (html [:div (map interested-people
                           (:app/list-interests (om/props this)))])))

(defn init-app 
  "Create a connection with schema, init the parser, reconciler, transact some data and
  add the app to the dom."
  []
  (let [conn       (d/create-conn schema)
        parser     (om/parser {:read read :mutate mutate})
        reconciler (om/reconciler {:state conn :parser parser})]
    (d/transact conn init-data)
    (om/add-root! reconciler RootView (gdom/getElement "app"))))

(enable-console-print!)
(init-app) ;; run the thing

