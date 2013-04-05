(use 'clojure.set)

;; Double Down: Write a function which doubles a number.
(defn double-down [x]
	(* x 2))

;; Hello World: Write a function which returns a personalized greeting.
(defn hello-world [name]
	(str "Hello, " name "!"))

;; Last Element: Write a function which returns the last element in a sequence.
(defn last-element [x]
	(first (reverse x)))

;; Penultimate Element: Write a function which returns the second to last element from a sequence.
(defn pen-elem [x]
	(nth x (- (count x) 2)))

;; Nth element: Write a function which returns the Nth element from a sequence.
(defn nth-elem [s x]
	(last (take (inc x) s)))

;; Count a Sequence: Write a function which returns the total number of elements in a sequence.
(defn count-sequence [xs]
	(reduce (fn [x y] (+ 1 x)) 0 xs))

;; Reverse a Sequence: Write a function which reverses a sequence.
(defn reverse-sequence [xs]
	(into '() xs))

;; Sum It All Up: Write a function which returns the sum of a sequence of numbers.
(defn sum-all [xs]
	(reduce + 0 xs))
 
;; Find the odd numbers: Write a function which returns only the odd numbers from a sequence.
(defn find-odd-nums [xs]
	(filter #(not= (mod % 2) 0) xs))

;; Fibonacci Sequence: Write a function which returns the first X fibonacci numbers.
(defn fib [n]
	(take n ((fn fib-recur [a b] (cons a (lazy-seq (fib-recur b (+ a b))))) 1 1)))

;; Palindrome Detector: Write a function which returns true if the given sequence is a palindrome.
(defn is-pal? [xs]
	(= (reverse xs) (seq xs)))

;; Flatten a Sequence: Write a function which flattens a sequence.
(defn flatten-sequence [n]
	(let [[x & xs] n]
		(cond
			(empty? n) '()
			(coll? x) (concat (flatten-sequence x) (flatten-sequence xs))
			:else (cons x (flatten-sequence xs)))))

;; Get the Caps: Write a function which takes a string and returns a new string containing only the capital letters.
(defn get-caps [s]
  (apply str (map char (filter #(and (<= 65 %) (<= % 90)) (map int s)))))

;; Pack a Sequence: Write a function which packs consecutive duplicates into sub-lists.
(defn pack-sequence [xs]
  (partition-by identity xs))

;; Duplicate a Sequence: Write a function which duplicates each element of a sequence.
(defn duplicate-sequence [xs]
	(reverse (reduce (fn [acc x] (conj acc x x)) '() xs)))
 
;; Replicate a Sequence: Write a function which replicates each element of a sequence a variable number of times.
(defn replicate-sequence [xs n]
	(reduce (fn [acc x] (concat acc (repeat n x))) '() xs))

;; Implement range: Write a function which creates a list of all integers in a given range.
(defn create-range [s e]
	(take (- e s) ((fn add-recur [a] (cons a (lazy-seq (add-recur (+ a 1))))) s)))

;; Maximum value: Write a function which takes a variable number of parameters and returns the maximum value.
(defn maximum [& args]
	(reduce (fn [x y] (if (> x y) x y)) 0 args))

;; Interleave Two Seqs: Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc.
(defn interleave-sequence [a b]
	(letfn [(irecur [xs ys]
		(if (or (empty? xs) (empty? ys))
			'()
			(let [[xh & xr] xs [yh & yr] ys] 
				(conj (irecur xr yr) yh xh))))]
		(irecur a b)))

;; Interpose a Seq: Write a function which separates the items of a sequence by an arbitrary value.
(defn interpose-sequence [sep a]
	(reduce (fn [x y] (conj x sep y)) [(first a)] (rest a)))

;; Drop Every Nth Item: Write a function which drops every Nth item from a sequence.
(defn drop-every-nth [a n]
	(let [zipped (zipmap (range) a)]
		(reverse (for [[k v] zipped :when (not= (mod (+ k 1) n) 0)] v))))

;; Factorial Fun: Write a function which calculates factorials.
(defn factorial [a]
	(reduce * 1 (range 1 (+ a 1))))
 
;; Reverse Interleave: Write a function which reverses the interleave process into x number of subsequences.
(defn reverse-interleave [a n]
  (partition (quot (count a) n) (apply interleave (partition n a))))

;; Rotate Sequence: Write a function which can rotate a sequence in either direction.
(defn rotate-sequence [n a]
	(let [series (reduce concat [] (take 10 (repeat a)))]
		(if (pos? n)
			(take (count a) (drop n series))
			(reverse (take (count a) (drop (* n -1) (reverse series)))))))

;; Flipping out: Write a higher-order function which flips the order of the arguments of an input function.
(defn flip-args [fun]
	(fn [a b] (fun b a)))

;; Split a sequence
(defn split-sequence [n a]
	(conj [] (take n a) (drop n a)))

;; Split by Type
(defn split-sequence-type [a]
	(map (fn [kv] (val kv)) (group-by type a)))

;; Longest Increasing Sub-Seq
(defn longest-inc-subseq [a]
	(letfn [(take-same [xs ys]
						(cond
							(or (empty? xs) (empty? ys)) []
							(= (first xs) (first ys)) (cons (first xs) (take-same (rest xs) (rest ys)))
							:else []))
					(isub-list [ls]
						(take-same ls (drop (first ls) (range))))]
		(if (empty? a) []
			(let [curr (isub-list a) recurs (longest-inc-subseq (rest a)) res (if (>= (count curr) (count recurs)) curr recurs)]
				(if (> (count res) 1) res [])))))

;; Partition a Sequence: Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned.
(defn partition-sequence [n a]
		(letfn [(phelper [c]
			(let [part (take n (drop (* c n) a))]
				(if (= n (count part)) (cons part (phelper (+ c 1))) [])))]
			(phelper 0)))

;; Count Occurrences: Write a function which returns a map containing the number of occurences of each distinct item in a sequence.
(defn count-occurances [a]
	(reduce (fn [acc x] (if (contains? acc x) (assoc acc x (+ (get acc x) 1)) (assoc acc x 1))) {} a))

;; Find Distinct Items: Write a function which removes the duplicates from a sequence. Order of the items must be maintained.
(defn distinct-items [a]
	(reverse (reduce (fn [acc x] (if (some #{x} acc) acc (cons x acc))) [] a)))

;; Function Composition: Write a function which allows you to create function compositions
(defn compose [& funs]
	(fn [& args] (first (reduce #(list (apply %2 %)) args (reverse funs)))))

;; Juxtaposition
(defn juxtapose [& funs]
	(fn [& args] (map #(apply % args) funs)))

;; Sequence Reductions: Like reduce, but returns intermediate results
(defn reducts
  ([f coll] (reducts f (first coll) (rest coll)))
  ([f val coll] 
     (cons val
           (lazy-seq
             (when-let [s (seq coll)]
               (reducts f (f val (first s)) (rest s)))))))

;; Write a function which takes a vector of keys and a vector of values and constructs a map from them
(defn create-map 
	([xs ys] (create-map xs ys {}))
	([[x & xs] [y & ys] map]
		(if (or (nil? x) (nil? y))
			map
			(create-map xs ys (assoc map x y)))))

;; Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence
(defn iterate-all [fun x]
	(cons x
		(lazy-seq 
			(iterate-all fun (fun x)))))

;; Given a function f and a sequence s, write a function which returns a map. The keys should be the values of f applied to each item in s
(defn group-bye 
	([fun xs] (group-bye fun xs {}))
	([fun [x & xs] groups]
		(if (nil? x)
			groups
			(let [result (fun x)]
				(group-bye fun xs 
					(if (contains? groups result)
						(assoc groups result (conj (get groups result) x))
						(assoc groups result [x])))))))

;; Black Box Testing
(defn black-box [coll]
	(#({{} :map #{} :set} (empty %) (if (reversible? %) :vector :list)) coll))

;; Given two integers, write a function which returns the greatest common divisor.
(defn gcd [x y]
	(let [smallest (if (< x y) x y)]
		(loop [curr smallest]
			(if (and (= (rem x curr) 0) (= (rem y curr) 0))
				curr
				(recur (dec curr))))))

;; Write a function which returns the first x number of prime numbers.
(defn primes [x]
	(loop [curr 2 primes []]
		(if (= (count primes) x)
			primes
			(if (not-any? #(= (rem curr %) 0) (range 2 curr))
				(recur (+ curr 1) (conj primes curr))
				(recur (+ curr 1) primes)))))

;; Merge with a function
(defn merge-with-fn [fun & maps]
	(reduce (fn [map1 map2] 
		(loop [[key & rest] (keys map2) acc map1]
			(if (nil? key) acc
				(recur rest 
					(if (contains? acc key)
						(assoc acc key (fun (get acc key) (get map2 key)))
						(assoc acc key (get map2 key))))))) maps))

;; Write a function that splits a sentence up into a sorted list of words.
(defn split-sort [s]
	(->> s
		(#(clojure.string/replace % #"[^A-Za-z\s]" ""))
		(#(clojure.string/split % #"\s"))
		(sort #(compare (clojure.string/upper-case %1) (clojure.string/upper-case %2)))))


;	(sort #(compare (clojure.string/upper-case %1) (clojure.string/upper-case %2))
;		(clojure.string/split
;			(clojure.string/replace s #"[^A-Za-z\s]" "")
;			#"\s")))

;; Tic-tac-toe Evaluator
(defn eval-ttt [[[a b c] [d e f] [g h i]]]
	(loop [[next & rest] [[a d g] [b e h] [c f i] [a e i] [c e g] [a b c] [d e f] [g h i]]]
		(cond 
			(nil? next) nil
			(every? #(= :x %) next) :x
			(every? #(= :o %) next) :o
			:else (recur rest))))

;; Filter Perfect Squares
(defn filter-perfect [int-str]
	(->> 
		int-str
		(#(clojure.string/split % #","))
		(map read-string)
		(filter #(= (mod (java.lang.Math/sqrt %) 1) 0.0))
		(reduce #(str %1 "," %2))))
