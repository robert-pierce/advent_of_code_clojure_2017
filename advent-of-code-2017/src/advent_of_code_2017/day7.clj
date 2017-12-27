(ns advent-of-code-2017.day7
  (:require [clojure.string :as str]))

(defn get-all-child-nodes
  [children-collection node-name tree-entry]
  (if-let [children (:children tree-entry )]
    (into children-collection children)
    children-collection))

(defn is-child?
  [all-children tree-entry]
  (let [is-child (some #(if (= % (key tree-entry)) true ) all-children)]
    is-child))

(defn find-root
  [tree]
  (let [all-children (reduce-kv get-all-child-nodes [] tree)]
    (seq (remove #(is-child? all-children %) tree))))

(defn parse-tree-reducer
  [coll entry]
  (let [node-raw (str/split (first entry) #" ")
        node-name (first node-raw)
        node-weight  (Integer/parseInt (str/replace (second node-raw) #"\(|\)" ""))  
        children (if-let [kids (second entry)]
                   (vec (map str/trim (str/split kids #",")))
                   nil)
        new-entry {:node-name node-name :node-weight node-weight :children children }]
    (assoc coll (:node-name new-entry) new-entry )))


(defn build-weight-map
  [active-node-name parsed-tree]
  (let [active-node (get parsed-tree active-node-name)
        node-weight (:node-weight active-node)
        children (:children active-node)]
    (if (nil? children)
      {active-node-name node-weight}
      (let [child-node-weights (vec (map #(build-weight-map % parsed-tree) children))]
        {active-node-name node-weight :child-weights child-node-weights}))))

(defn extract-leaves
"ADDED A FLATTEN"
  [weight-map parent]
  (let [node-name  (first (keys weight-map))
        node-weight (val (first weight-map))
        children (:child-weights weight-map)]
    (if (nil? children)
      (do 
        (assoc weight-map :parent parent))
      (do
        (flatten (map #(extract-leaves % node-name) children))))))

(defn analyze-leaves-reducer
  [coll leaf]
  (let [parent (:parent leaf)
        value (val (first leaf))
        current-weights (get coll parent)
        updated-weights (conj current-weights value)]
    (assoc coll parent updated-weights)))

(defn get-leaves-difference
  [values]
  (let [freq (frequencies values)
        distinct-values (distinct values)
        different-node (first (->> freq
                               (group-by val)
                               (#(get % 1))
                               (map key)))
        difference (first (remove #(= % 0) 
                                  (map #(- % different-node) distinct-values)))]
    difference))

(defn process-branch
  "NEED TO WORK ON THIS Replacing analyze-leave, processing 
  aggregated leaves from analyze-leaves-2. Need to do "
  [coll branch]
  (let [name (key branch)
        values (val branch)
        balanced? (apply = values)
        ]
    (if balanced? 
      (assoc coll name (reduce + values))
      (assoc coll name {:different (get-leaves-difference values)}))))

(defn analyze-leaves
  [leaves]
  (let [aggregated-leaves (reduce analyze-leaves-reducer {} leaves)]
    (reduce process-branch {} aggregated-leaves)))


(defn remove-leaves-reducer
  [tree children]
  (apply #(dissoc tree %) children))

(defn get-children-to-remove
  [tree analyzed-leaves]
  (let [parent-keys-to-collapse (map :parent analyzed-leaves)
        nodes-to-collapse-on (map #(get tree %) parent-keys-to-collapse)
        children (map :children nodes-to-collapse-on)]
    (flatten children)))

(defn collapse-tree-reducer
  [tree analyzed-leaf]
  (let [parent-key-to-collapse (:parent analyzed-leaf)
        sum-of-children (:sum analyzed-leaf)
        node-to-collapse-on (get tree parent-key-to-collapse)
        current-node-weight (:node-weight node-to-collapse-on)
        node-to-collapse-children (:children node-to-collapse-on)
        collapsed-node (assoc node-to-collapse-on 
                         :node-weight (+ sum-of-children current-node-weight)
                         :children nil)]
    (assoc tree parent-key-to-collapse collapsed-node)))

(defn process-move
"FIGURE OUT AT ANALYZE LEAVES"
  [root analysis-map]
  (let [old-weight-map (:weight-map analysis-map)
        old-tree (:tree analysis-map)
        extracted-leaves  (extract-leaves old-weight-map "root")
        analyzed-leaves (map analyze-leaves extracted-leaves)
        
        difference (for [entry analyzed-leaves
                         :let [difference (:difference entry)]
                         :when (not (:balanced entry))]
                     difference)]
    (if (not (empty? difference))
      (assoc analysis-map :difference (first difference))
      (do
        (let [collapsed-tree (reduce collapse-tree-reducer old-tree analyzed-leaves)
              children-to-remove (get-children-to-remove old-tree analyzed-leaves)
              collapsed-tree-with-leaves-removed (apply dissoc collapsed-tree children-to-remove)
              updated-weight-map (build-weight-map root collapsed-tree-with-leaves-removed)]
          {:tree collapsed-tree-with-leaves-removed :weight-map updated-weight-map :difference nil}
          )))))

(defn process-unbalanced-tree
  [initial-parsed-tree initial-weight-map root]
  (loop [analysis-map {:tree initial-parsed-tree
                       :weight-map initial-weight-map
                       :difference nil}]
    (if (not (nil? (:difference analysis-map)))
      (:difference analysis-map)
      (recur (process-move root analysis-map)))))


(defn day-7-part-1
 [body]
 (let [str-body (slurp body)
       split-on-rows (str/split str-body #"\n")
       split-on-arrows (map #(str/split % #"->") split-on-rows)
       parsed-tree (reduce parse-tree-reducer {} split-on-arrows)
       root (find-root parsed-tree)]
   {:status 200 :body (str "The root is " root)}))

(defn day-7-part-2
  [body]
  (let [str-body (slurp body)
        split-on-rows (str/split str-body #"\n")
        split-on-arrows (map #(str/split % #"->") split-on-rows)
        parsed-tree (reduce parse-tree-reducer {} split-on-arrows)
        root (find-root parsed-tree)
        weight-map (build-weight-map (key (first root)) parsed-tree)
         ]
    {:status 200 :body (str "The answer is " weight-map )}))
