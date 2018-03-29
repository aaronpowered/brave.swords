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

(defn fancy [sample]
 (loop [x [0      []    0    false      2        ]  ]
  (let [  [index  coll  lvl  in-a-str?  line-type] x
        exist (< index (count sample))
        c (when exist (nth sample index))
        last-c (when-not (= index 0) (nth sample (dec index))) 
        new-line (if (= lvl 0) [] [\newline])
        line (fn [lc nc l] (if (and (= last-c lc) (= c nc)) l line-type))
        line-type (line \[ \{ 1)
        line-type (line \} \] 2)
        lvl ((case c \{ + \} - \[ + \] - (fn [x y] x)) lvl line-type)
        ext (repeat lvl \space)
        c (if-not in-a-str?
            (let [new-line (apply conj new-line ext)]
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

(defn round [x & {p :precision}] (if p (let [scale (Math/pow 10 p)] (-> x (* scale) Math/round (/ scale))) (Math/round x)))
(defn mapfun "map a function on all map values" [m f] (into {} (for [[k v] m] [k (f v)])))
(defn title! [t] (set! (. js/document -title) t))
(defn by-id "Short-hand for document.getElementById(id)" [id] (.getElementById js/document id))
(defn val-by-id [id] (if id (.-value (by-id id)) (js/console.log (str "No id:"id))))
(defn set-val! [id update] (if id (set! (.-value (by-id id)) update) (js/console.log (str "Can't set: "id))))
(defn set-display! [id update] (set! (.-style.display (by-id id)) update))
(defn set-html! [id s] (set! (.-innerHTML (by-id id)) s))

(defn jsx->clj [x] (into {} (for [k (.keys js/Object x)] [k (aget x k)])))
(defn obj->clj
  [obj]
  (-> (fn [result key]
        (let [v (aget obj key)]
          (if (= "function" (goog/typeOf v))
            result
            (assoc result key v))))
      (reduce {} (.getKeys goog/object obj))))

(defn user-agent [] (-> js/navigator .-userAgent))
(defn domain [] (-> js/document .-domain))
(defn title [] (-> js/document .-title))
(defn referrer [] (-> js/document .-referrer))
(defn prev-sites [] (-> js/history .-length))
(defn client-time [] (.Date js/window))
(defn timezone [] (/ (.getTimezoneOffset (client-time)) 60))
(defn protocol [] (-> js/window .-location .-protocol))
(defn host [] (-> js/window .-location .-host))
(defn host-name [] (-> js/window .-location .-hostname))
(defn url-hash [] (-> js/window .-location .-hash))
(defn url-path [] (-> js/window .-location .-pathname))
(defn browser [] (-> js/navigator .-appCodeName))
(defn browser-name [] (-> js/navigator .-appName))
(defn browser-engine [] (-> js/navigator .-product))
(defn browser-version [] (-> js/navigator .-appVersion))
(defn browser-plugins [] (obj->clj (-> js/navigator .-plugins)))
(defn browser-mimes [] (obj->clj (-> js/navigator .-mimeTypes)))
(defn browser-lang [] (-> js/navigator .-language))
(defn browser-languages [] (js->clj (-> js/navigator .-languages)))
(defn charset [] (-> js/document .-charset))
(defn online? [] (-> js/navigator .-onLine))
(defn connection [] (obj->clj (-> js/navigator .-connection)))
(defn os [] (-> js/navigator .-platform))
(defn cpu-cores [] (-> js/navigator .-hardwareConcurrency))
(defn performance-timing [] (obj->clj (-> js/window .-performance .-timing)))
(defn performance-memory [] (obj->clj (-> js/window .-performance .-memory)))
(defn media-devices [] (obj->clj (-> js/navigator .-mediaDevices)))
(defn cookies? [] (-> js/navigator .-cookieEnabled))
(defn cookies [] (-> js/document .-cookie))
(defn local-storage [] (js/localStorage))
(defn viewport [] (obj->clj (-> js/document .-visualViewport)))
(defn screen-width [] (-> js/screen .-width))
(defn screen-height [] (-> js/screen .-height))
(defn available-width [] (-> js/screen .-availWidth))
(defn mobile? [] (< (available-width) 768))
(defn tablet? [] (= (available-width) 768))
(defn pc? [] (> (available-width) 768))
(defn available-height [] (-> js/screen .-availHeight))
(defn color-depth [] (-> js/screen .-colorDepth))
(defn pixel-depth [] (-> js/screen .-pixelDepth))
(def check-in 
  {:path (url-path)
   :hash (url-hash)
   :referrer (referrer)})
(def device-info 
  {:os (os)
   :lang (browser-lang)
   :cpucores (cpu-cores)
   :useragent (user-agent)
   :screenwidth (screen-width)
   :screenheight (screen-height)
   :availablewidth (available-width)
   :availableheight (available-height)
   :colordepth (color-depth)
   :pixeldepth (pixel-depth)})

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
                  (if (= c hover)
                    (do (function)) ;(js/window.alert "Y"))
                     (recur))))))))

(defn leave [id function]
      (let [hover (events->chan (js/document.getElementById id)
                   EventType.MOUSELEAVE)]
            (go (while true
                 (loop [] (let [[v c] (alts! [hover])]
                  (if (= c hover)
                    (do (function)) ;(js/window.alert "Y"))
                     (recur))))))))
