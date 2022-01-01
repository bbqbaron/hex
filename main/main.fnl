;; i'm neither luaist nor fennelist, missing clojure a bit.
;; this will warp my code, i'm sure.
(fn group-by [f vs]
  (let [s {}]
    (each [k v (pairs vs)]
      (let [uk (f v)
            found (or (. s uk) {})]
        (table.insert found v)
        (tset s uk found)))
    s))

(fn empty? [xs] (<= (length xs) 0))

(fn range [n]
  (var out [])
  (for [i 1 n]
    (table.insert out i))
  out)

(fn some [f xs]
  (var out nil)
  (each [k v (pairs xs) :until out]
    (set out (f v)))
  out)

(fn map [f xs]
  (var out [])
  (each [k v (ipairs xs)]
    (table.insert out (f v)))
  out)

(fn mapcat [f xs]
  (var out [])
  (each [k v (ipairs xs)]
    (each [k2 v2 (ipairs (f k v))]
      (table.insert out v2)))
  out)

(fn filter [f xs]
  (icollect [_ v (ipairs xs)]
    (when (f v) v)))

(fn first [xs] (. xs 1))

(fn seek [f xs]
  (var out nil)
  (each [_ v (ipairs xs) :until out]
    (when (f v) (set out v)))
  out)

(fn pos? [x] (> x 0))

(fn every? [f xs]
  (accumulate [ok true _ v (pairs xs)]
    (and ok (f v))))

(fn reset [{: turn : hexes &as self}]
  (set self.turn "blue")
  (each [_ qs (pairs hexes)]
    (each [_ rs (pairs qs)]
      (each [_ {: id &as hex} (pairs rs)]
        (set hex.owner nil)
        (sprite.play_flipbook id "empty")))))

(fn inc [x] (+ x 1))
(fn dec [x] (- x 1))

(fn set-in [m [k & ks] v]
  (var m (or m {}))
  (if (empty? ks)
      (tset m k v)
      (let [v2 (or (. m k) {})] 
        (tset m k v2)
        (set-in v2 ks v)))
  m)

;; is there some generic value hash for colls? no?
(local HexSet {})

(fn HexSet.contains [self [q r s]]
  (not (= nil (?. self.data q r s))))

(fn HexSet.conj [self [q r s]]
  (when (not (?. self.data q r s))
    (set-in self.data [q r s] true)))

(fn to-hexset [xs]
  (var out {:data {}})
  (each [_ [q r s] (pairs xs)]
    (when (not (?. out q r s))
      (set-in out.data [q r s] true)))
  (setmetatable out {:__index HexSet})
  out)

(fn raw-neighbors [[x y z]]
  [[(inc x) y (dec z)]
   [(dec x) y (inc z)]
   [x (inc y) (dec z)]
   [x (dec y) (inc z)]
   [(inc x) (dec y) z]
   [(dec x) (inc y) z]])

(fn foreach [f xs]
  (each [_ v (ipairs xs)]
    (f v)))

(fn valid? [[q r s]]
  (and
   (= 0 (+ q r s))
   (and
    (<= 0 q 10)
    (<= 0 r 10)
    (<= -20 s 0))))

(fn find-path-from [{ : turn : hexes} from]
  (let [seen (to-hexset [])]
    (fn step [[q r s &as pt]]
      (when (and (valid? pt) (not (seen:contains pt))
                 (= turn (. hexes q r s :owner)))
        (seen:conj pt)
        (foreach step (raw-neighbors pt))))
    (step from)
    (match turn
      :red (every?
            (fn [r]
              (some
               (fn [rs]
                 (. rs r))
               seen.data))
            (map dec (range 11))
            )
      :blue (every?
             (fn [q]
               (when (. seen.data q)                    
                    (let [rs (. seen.data q)]                    
                      rs)))
            (map dec (range 11))))))

(fn find-path [{: turn &as self}]
  (some
   (partial find-path-from self)
   (match turn
     :blue (map
           (fn [r]
             [0 r (* -1 r)])
           (map dec (range 11)))
     :red (map
            (fn [q]
              [q 0 (* -1 q)])
            (map dec (range 11))))))

(fn every-pred? [fns]
  (fn [x]
    (every? #($ x) fns)))

(fn neighbors [seen pt]
  (filter (every-pred?
           [(fn [x] (not (seen:contains x)))
            valid?])
          (raw-neighbors pt)))

(fn pprint [t]
  (each [k v (pairs t)]
    (print k v)))

;; bad perf but only done once for a small grid
(fn walk-hexes [seen pt]
  (let [coords []]
    (fn step [pt]
      (when (and (valid? pt) (not (seen:contains pt)))
        (table.insert coords pt)
        (seen:conj pt)
        (let [ns (raw-neighbors pt)]
          (each [_ v (pairs ns)]
            (step v)))))
    (when (valid? pt) (step pt))
    coords))

(fn hex-pos [[q r s]]
  [(+ q (* r 0.5)) (* r 0.75)])

(fn test-hexes [owner]
  (accumulate [t {}
               _ v (ipairs (walk-hexes (to-hexset []) [0 0 0]))]
    (set-in t v {:owner owner})))

;; ....really?
(fn round [n]
  (if (>= n (+ (math.floor n) 0.5))
      (math.ceil n)
      (math.floor n)))

;; TODO undo hardcoding of dimensions
;; TODO learn geometry

(fn in-triangle [[[lx ly] [cx cy] [rx ry]] [x y]]
  ;; TODO is there a canonical trig fn for this?
  (and
   (<= lx x rx)
   (<= (math.min ly cy) y (math.max ly cy))
   (let [yfactor (/
          (math.abs (- cy y))
          (math.abs (- cy ly)))]
     (<=
      (- cx (* yfactor (/ (- rx lx) 2)))
      x
      (+ cx (* yfactor (/ (- rx lx) 2)))))))

(fn comp [fs]
  (fn [x]
    (var st x)
    (for [i (length fs) 1 -1]
      (set st ((. fs i) st)))
    st))

(fn acc [f i xs]
  (var st i)
  (each [_ v (ipairs xs)]
    (set st (f v))))

(local scale 1.6)
(local tile-size (* 32 scale))
;; w, h = window.get_size() TODO this seems to return monitor size on macos, not sure of intent of this fn
(local w 960)
(local h 640) ;; TODO hardcoded
(local board-width (* 11 1.5 tile-size))
(local board_height (* 11 0.75 tile_size))
(local vmargin (/ (- h board_height) 2))
(local hmargin (/ (- w board_width) 2))

(local hex-body (* scale 23))
(local hex-triangle-height (* scale 9))
(local half-tile (/ tile-size 2))

(fn in-hex [hp [x y &as pt]]
  (let [[hx hy] (map (partial * tile-size) (hex-pos hp))]
    (and
     (<= hx x (+ hx tile-size))
     (<= hy y (+ hy tile-size))
     (or
      (in-triangle [[hx (+ hy hex-body)]
                    [(+ hx (/ tile-size 2)) (+ hy tile-size)]
                    [(+ hx tile-size) (+ hy hex-body)]]
                   pt)
      (in-triangle [[hx (+ hy hex-triangle-height)]
                    [(+ hx (/ tile-size 2)) hy]
                    [(+ hx tile-size) (+ hy hex-triangle-height)]]
                   pt)
      (and
       (<= hx x (+ hx tile-size))
       (<= (+ hy hex-triangle-height) y (+ hy hex-triangle-height hex-body)))))))

(fn hex-for [[x y &as pt]]
  (seek
   #(in-hex $ pt)
   (filter valid?
           (let [
                 rbase (math.floor (/ y hex-body))
                 rs [(dec rbase) rbase]
                 qbase (math.floor (/ (- x (* rbase half-tile)) half-tile))
                 qt (math.floor (/ qbase 2))]
             (if (= 0 (% qbase 2))
                 [[qt rbase (- (+ qt rbase))]
                  [qt (dec rbase) (- (+ qt (dec rbase)))]]
                 [[qt rbase (- (+ qt rbase))]
                  [(inc qt) (dec rbase) (- (+ qt rbase))]])))))

(fn next-turn [{: turn &as self}]
  (when (find-path self)
    (tset self.score turn (inc (. self.score turn)))
    (reset self)
    (msg.post "/main#score" "score" self.score))
  (set self.turn (match turn :red :blue _ :red)))

(fn edges [hexes]
  (for [q 0 10]
    (let [p (go.get_position (. hexes q 0 (- q) :id))]
      (set p.x (- p.x 28))
      (set p.y (+ p.y 28))
      (set p.z -1)
      (let [id (factory.create "/main#hex" p nil nil 1.75)]
        (go.set (msg.url "main" id "sprite") "tint.w" 0.5)
        (sprite.play_flipbook id "red")))
    (let [p (go.get_position (. hexes q 10 (- 0 q 10) :id))]
      (set p.x (+ p.x 28))
      (set p.y (- p.y 28))
      (set p.z -1)
      (let [id (factory.create "/main#hex" p nil nil 1.75)]        
        (go.set (msg.url "main" id "sprite") "tint.w" 0.5)
        (sprite.play_flipbook id "red"))))
  (for [r 0 10]
    (let [p (go.get_position (. hexes 0 r (- r) :id))]
      (set p.x (- p.x 28))
      (set p.y (- p.y 28))
      (set p.z -1)
      (let [id (factory.create "/main#hex" p nil nil 1.75)]        
        (go.set (msg.url "main" id "sprite") "tint.w" 0.5)
        (sprite.play_flipbook id "blue")))
    (let [p (go.get_position (. hexes 10 r (- 0 r 10) :id))]
      (set p.x (+ p.x 28))
      (set p.y (+ p.y 28))
      (set p.z -1)
      (let [id (factory.create "/main#hex" p nil nil 1.75)]        
        (go.set (msg.url "main" id "sprite") "tint.w" 0.5)
        (sprite.play_flipbook id "blue")))))

(local base-hexes (walk-hexes (to-hexset []) [0 0 0]))

(fn init [self]
  (msg.post "." "acquire_input_focus")
  (set self.turn "red")
  (set self.hexes {})
  (set self.score {:red 0 :blue 0})
  (msg.post "/main#score" :score {:red 0 :blue 0})
  (each [k [q r s &as v] (pairs base-hexes)]
    (let [[hx hy] (hex-pos v)
          px (+ (* tile-size 0.5) hmargin (* hx tile-size))
          py (- h vmargin (* hy tile-size))]
      (set-in self.hexes [q r s]
              {:id (factory.create "/main#hex" (vmath.vector3 px py 0) nil nil scale)})))
  (edges self.hexes))

(fn on-input [{: hexes : turn &as self} action_id action]
  (when (and (= action_id (hash "touch")) action.released)
    (let [p (hex-for [(- action.x hmargin) (- h vmargin action.y (- (* tile-size 0.5)))])]
      (when p
        (let [[q r s] p
              {: id &as hex} (. hexes q r s)]
          (when (= nil hex.owner)
            (set hex.owner turn)
            (sprite.play_flipbook id turn)
            (next-turn self)))))))

{:find_path find-path : reset :hex_pos hex-pos :base_hexes base-hexes :hex_for hex-for :next_turn next-turn
 : edges : init :on_input on-input}
