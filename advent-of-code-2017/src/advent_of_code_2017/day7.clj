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
  [tree weight-map parent]
  (let [node-name  (first (keys weight-map))
        node-weight (val (first weight-map))
        children (:child-weights weight-map)]
    (if (nil? children)
      (do 
        (assoc weight-map :parent parent :parent-weight (:node-weight (get tree parent))))
      (do
        (map #(extract-leaves tree % node-name) children)))))

(defn indices-of [f coll]
  (keep-indexed #(if (f %2) %1 nil) coll))

(defn first-index-of [f coll]
  (first (indices-of f coll)))

(defn find-thing [value coll]
  (first-index-of #(= % value) coll))

(defn get-different-node
  [frequencies]
  (first (->> frequencies
              (group-by val)
              (#(get % 1))
              (map key))))

(defn get-leaves-difference
  [values leaf-names]
  (let [freq (frequencies values)
        distinct-values (distinct values)
        different-node (get-different-node freq)
        index (find-thing different-node values)
        different-node-name (nth (vec leaf-names) index)
        difference (first (remove #(= % 0)
                                  (map #(- % different-node) distinct-values)))]
    {:difference difference :leaf-name different-node-name}))

(defn process-branch
  [coll branch]
  (let [name (key branch)
        parent-weight (:parent-weight (val branch))
        leaf-names (:leaf-names (val branch))
        values (:updated-weight (val branch))
        balanced? (apply = values)]
    (if balanced? 
      (assoc coll name {:leaf-names leaf-names :updated-weight (reduce + values) :parent-weight parent-weight })
      (assoc coll name {:leaf-names leaf-names :updated-weight {:different (get-leaves-difference values leaf-names)} :parent-weight parent-weight} ))))

(defn analyze-leaves-reducer
  [coll leaf]
  (let [parent (:parent leaf)
        parent-weight (:parent-weight leaf)
        leaf-name (key (first leaf))
        value (val (first leaf))
        current-leaves (:leaf-names (get coll parent))
        updated-leaves (conj current-leaves leaf-name)
        current-weights (:updated-weight (get coll parent))
        updated-weights (conj current-weights value)]
    (assoc coll parent {:leaf-names updated-leaves :updated-weight  updated-weights :parent-weight parent-weight})))

(defn analyze-leaves
  [leaves]
  (let [aggregated-leaves (reduce analyze-leaves-reducer {} leaves)]
     (reduce process-branch {} aggregated-leaves)))


(defn collapse-tree-reducer
  [tree analyzed-leaf]
  (let [parent-key-to-collapse (key analyzed-leaf)
        sum-of-children (:updated-weight (val analyzed-leaf))
        node-to-collapse-on (get tree parent-key-to-collapse)
        current-node-weight (:node-weight node-to-collapse-on)
        children-to-collapse (:leaf-names (val analyzed-leaf))
        node-to-collapse-children (get node-to-collapse-on :children)
        trimmed-children (remove (set children-to-collapse) node-to-collapse-children)
        collapsed-node (assoc node-to-collapse-on 
                         :node-weight (+ sum-of-children current-node-weight)
                         :children (if (= (count trimmed-children) 0) nil trimmed-children))
        tree-with-trimmed-leaves (reduce #(dissoc %1 %2) tree children-to-collapse)]
    (assoc tree-with-trimmed-leaves parent-key-to-collapse collapsed-node)))

(defn extract-difference-reducer
  [coll leaf]
  (let [parent-name (key leaf)
        leaf-val (val leaf)
        updated-weight (:updated-weight leaf-val)
        parent-weight (:parent-weight leaf-val)
        is-map? (map? updated-weight)]
    (if is-map? 
      (assoc coll :answer 
             {:node parent-name 
              :node-weight parent-weight 
              :difference updated-weight})
      coll)))

(defn process-move
  [root analysis-map]
  (let [old-weight-map (:weight-map analysis-map)
        old-tree (:tree analysis-map)
        extracted-leaves  (flatten (extract-leaves old-tree old-weight-map "root"))
        analyzed-leaves (analyze-leaves extracted-leaves)
        difference (reduce extract-difference-reducer {} analyzed-leaves)]
    (if (not (empty? difference))
      (do
        (assoc analysis-map :difference difference))
      (do
        (let [collapsed-tree (reduce collapse-tree-reducer old-tree analyzed-leaves)
              updated-weight-map (build-weight-map root collapsed-tree)]
          {:tree collapsed-tree :weight-map updated-weight-map :difference nil})))))

(defn process-unbalanced-tree
  [initial-parsed-tree initial-weight-map root]
  (loop [analysis-map {:tree initial-parsed-tree
                       :weight-map initial-weight-map
                       :difference nil}]
    (if (not (nil? (:difference analysis-map)))
      (:difference analysis-map)
      (recur (process-move root analysis-map)))))

(defn process-tree
  [initial-parsed-tree intial-weight-map root]
  (let [answer (process-unbalanced-tree initial-parsed-tree intial-weight-map root)
        culprit-leaf  (get-in answer [:answer :difference :different :leaf-name])
        culprit-leaf-original-value (get-in initial-parsed-tree [culprit-leaf :node-weight])
        culprit-leaf-difference (get-in answer [:answer :difference :different :difference])
        final-leaf-value (+ culprit-leaf-difference culprit-leaf-original-value)
        final-answer {:node-name culprit-leaf :new-balanced-value final-leaf-value}]
   final-answer))


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
        root (key (first (find-root parsed-tree)))
        weight-map (build-weight-map root parsed-tree)
        answer  (process-tree parsed-tree weight-map root)]
    {:status 200 :body (str "The answer is " answer )}))
