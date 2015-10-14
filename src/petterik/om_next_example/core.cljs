(ns petterik.om_next_example.core
  (:require [goog.dom :as gdom]
            [sablono.core :as html :refer-macros [html]]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [datascript.core :as d]))

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

(defmulti mutate om/dispatch)
(defmulti read om/dispatch)

(defmethod read :default
  [{:keys [state component selector] :as env} key params]
  (prn (om/props component))
  (let [sel (if selector {key selector} key)]
    {:value (or (-> (d/pull (d/db state) [sel] (om/ident component (om/props component)))
                    (get key))
                (get (om/props component) key))}))

(defmethod read :app/list-interests [{:keys [state selector]} _ _] 
  {:value (d/q '[:find [(pull ?interest ?selector) ...]
                 :in $ ?selector
                 :where [?interest :interest]]
               (d/db state)
               selector)})

(defmethod mutate 'person/dislike [{:keys [state]} _ {:keys [entity interest]}]
  {:value [:interest :person/likes]
   :action 
   #(let [res (d/transact! state [[:db/retract (:db/id entity) 
                                   :person/likes [:interest interest]]])]
      res)})

(defui Person
  static om/Ident
  (ident [this {:keys [person/name]}]
         [:person/name name])
  static om/IQuery
  (query [this]
         [:db/id 
          :person/name 
          :person/twitter 
          {:person/likes [:interest]}])
  Object
  (render [this]
          (let [{:keys [like
                        person/name person/twitter 
                        person/likes] :as entity} (om/props this)]
            (prn {:person name :person-likes like})
            (html [:div 
                   [:p (str twitter)]
                   [:button {:on-click 
                             #(om/transact! this `[(person/dislike {:entity ~entity
                                                                    :interest ~like})])}
                    "dislike"]]))))

(def person (om/factory Person {:keyfn :person/name}))

(defui InterestedPeople
  static om/IQuery
  (query [this]
         [:db/id 
          :interest
          {:person/_likes (om/get-query Person)}])
  static om/Ident
  (ident [this {:keys [interest]}]
         [:interest interest])
  Object
  (render [this]
          (let [{:keys [db/id interest person/_likes]} (om/props this)]
            (html
              [:div [:h2 (str "People who like " (name interest) ":")]
                    [:div (map #(person (assoc % :like interest)) _likes)]]))))

(def interested-people (om/factory InterestedPeople))

(defui RootView
  static om/IQuery
  (query [this]
         [{:app/list-interests (om/get-query InterestedPeople)}])
  Object
  (render [this]
          (let [{:keys [app/list-interests]} (om/props this)]
            (html
              [:div (map interested-people list-interests)]))))


(defn init-app []
  (prn "init")
  (let [conn       (d/create-conn schema) 
        parser     (om/parser {:read read :mutate mutate}) 
        reconciler (om/reconciler {:state conn :parser parser})] 
      (d/transact conn init-data)
      (om/add-root! reconciler RootView (gdom/getElement "app"))))

(enable-console-print!)
(init-app)

