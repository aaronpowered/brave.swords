(ns brave.swords
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [>! <! put! chan alts!]]
            [goog.events :as events]
            [goog.string :as gstr])
  (:import [goog.events EventType]))

(defn nil->str [x] (if (or (undefined? x) (nil? x)) "nil" x))
(defn format* [fmt args] (let [fmt  (or fmt "") args (mapv nil->str args)] (apply gstr/format fmt args)))
(defn formato [fmt & args] (format* fmt args))
(defn drop-nth [n coll] (keep-indexed #(if (not= %1 n) %2) coll))
(defn deaccent [input]
  (reduce (fn [s [pat repl]]
            (clojure.string/replace s pat repl))
          input
          [[#"[éèêë]" "e"]
           [#"[á]"    "a"]
           [#"[űúü]"  "u"]
           [#"[őöó]"  "o"]
           [#"[í]"    "i"]
           ]))

(defn fancy "Recursive Pretty Print limited to maps and vectors."
  [sample]
 (loop [x [0      []    0    false      2        ]  ]
  (let [  [index  coll  lvl  in-a-str?  line-type] x
        character-exist (< index (count sample))
        c (when character-exist (nth sample index))
        last-c (when-not (= index 0) (nth sample (dec index)))
        prev? (fn [lc nc] (and (= last-c lc) (= c nc)))
        line-type (cond
                    (prev? \[ \{) 1
                    (prev? \} \]) 2
                    :else line-type)
        lvl (if (= last-c nil) 0
              ((case c \{ + \} - \[ + \] - (fn [x y] x)) lvl line-type))
        c (if-not in-a-str?
            (let [new-line (apply conj [\newline] (repeat lvl \space))]
              (case c
                \newline ""
                \, new-line 
                \{ (if (= last-c \[) \{ (conj new-line \{))
                \[ (if (= last-c \[) \[ (conj new-line \[))
                c))
            c)
        coll (if (vector? c) (apply conj coll c) (conj coll c))
        in-a-str? (if (and (= c \") (not= last-c \\)) (if in-a-str? false true) in-a-str?)
        return [(inc index) coll lvl in-a-str? line-type]]
  (if c (recur return)
    (apply str coll)))))

(defn deep-merge "Excellent merge. Credits for Metosin Potpurri" 
  [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (when (some identity vs)
      (reduce #(rec-merge %1 %2) v vs))))

(defn round [x & {p :precision}] (if p (let [scale (Math/pow 10 p)] (-> x (* scale) Math/round (/ scale))) (Math/round x)))
(defn mapfun "map a function on all map values" [m f] (into {} (for [[k v] m] [k (f v)])))
(defn title! [t] (set! (. js/document -title) t))
(defn by-id "Short-hand for document.getElementById(id)" [id] (.getElementById js/document id))
(defn val-by-id [id] (if id (.-value (by-id id)) (js/console.log (str "No id:"id))))
(defn set-val! [id update] (if id (set! (.-value (by-id id)) update) (js/console.log (str "Can't set: "id))))
(defn set-display! [id update] (set! (.-style.display (by-id id)) update))
(defn set-html! [id s] (set! (.-innerHTML (by-id id)) s))

(defn jsx->clj "Swiss knife v1"
  [x] 
  (into {} (for [k (.keys js/Object x)] [k (aget x k)])))

(defn obj->clj "Swiss knife v2"
  [obj]
  (-> (fn [result key]
        (let [v (aget obj key)]
          (if (= "function" (goog/typeOf v))
            result
            (assoc result key v))))
      (reduce {} (.getKeys goog/object obj))))

(def client-profile "Everything I know about"
  {:useragent (-> js/navigator .-userAgent)
   :domain (-> js/document .-domain)
   :title (-> js/document .-title)
   :referrer (-> js/document .-referrer)
   :prevsites (-> js/history .-length)
   :clienttime (.Date js/window)
   :timezone (/ (.getTimezoneOffset (client-time)) 60)
   :protocol (-> js/window .-location .-protocol)
   :host (-> js/window .-location .-host)
   :hostname (-> js/window .-location .-hostname)
   :hash (-> js/window .-location .-hash)
   :path (-> js/window .-location .-pathname)
   :browser (-> js/navigator .-appCodeName)
   :browsername (-> js/navigator .-appName)
   :browserengine (-> js/navigator .-product)
   :browserversion (-> js/navigator .-appVersion)
   :browserplugins (obj->clj (-> js/navigator .-plugins))
   :browsermimes (obj->clj (-> js/navigator .-mimeTypes))
   :lang (-> js/navigator .-language)
   :browser-languages (js->clj (-> js/navigator .-languages))
   :charset (-> js/document .-charset)
   :online? (-> js/navigator .-onLine)
   :connection (obj->clj (-> js/navigator .-connection))
   :os (-> js/navigator .-platform)
   :cpucores (-> js/navigator .-hardwareConcurrency)
   :performancetiming (obj->clj (-> js/window .-performance .-timing))
   :performancememory (obj->clj (-> js/window .-performance .-memory))
   :mediadevices (obj->clj (-> js/navigator .-mediaDevices))
   :cookies? (-> js/navigator .-cookieEnabled)
   :cookies (-> js/document .-cookie)
   :localstorage (js/localStorage)
   :viewport (obj->clj (-> js/document .-visualViewport))
   :screenwidth (-> js/screen .-width)
   :screenheight (-> js/screen .-height)
   :availablewidth (-> js/screen .-availWidth)
   :availableheight (-> js/screen .-availHeight)
   :colordepth (-> js/screen .-colorDepth)
   :pixeldepth (-> js/screen .-pixelDepth)})

(def check-in 
  (select-keys [:path :hash :referrer] 
               client-profile))

(def device-info 
  (select-keys [:os :lang :cpucores :useragent
                :screenwidth :screenheight :availablewidth :availableheight
                :colordepth :pixeldepth] 
               client-profile))

(defn events->chan
  "Given a target DOM element and event type return a channel of
  observed events. Can supply the channel to receive events as third
  optional argument."
  ([el event-type] (events->chan el event-type (chan)))
  ([el event-type c]
   (events/listen el event-type
     (fn [e] (put! c e)))
   c))

(defn on-click [id event]
  (when-let [target-el (by-id id)]
  (.addEventListener target-el "click" event)))

(defn on-key-press [id event]
  (when-let [target-el (by-id id)]
  (.addEventListener target-el "keydown" event)))

(defn dom-ready [event]
  (.addEventListener js/document "DOMContentLoaded" event))

(defn hover [id function]
      (let [hover (events->chan (js/document.getElementById id)
                   EventType.MOUSEENTER)]
            (go (while true
                 (loop [] (let [[v c] (alts! [hover])]
                  (if (= c hover) (function) (recur))))))))

(defn leave [id function]
      (let [hover (events->chan (js/document.getElementById id)
                   EventType.MOUSELEAVE)]
            (go (while true
                 (loop [] (let [[v c] (alts! [hover])]
                  (if (= c hover) (function) (recur))))))))
