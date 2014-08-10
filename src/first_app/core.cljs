(ns first-app.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def sound (js/Audio. "blip2.mp3"))
(def initial-bpm 120)
(def delay-times {:2 120 :4 60 :8 30 :16 15})

(def app-state (atom {:bpm initial-bpm
                      :playing false
                      :beat-duration 4
                      :beats-per-bar 4
                      :current-beat -1
                      }))

(def bip #js          ["square",0.0000,0.4000,0.0000,0.1340,0.3180,0.1520,905.0000,432.0000,2000.0000,-0.9680,0.1280,0.0000,10.0879,0.0003,0.0000,0.6020,0.3160,0.0000,-0.4040,0.0000,0.0020,0.0000,1.0000,-0.6440,0.5200,0.0000,0.0000])
(def first-bar-bip #js ["square",0.0000,0.4000,0.0000,0.1340,0.3600,0.1520,905.0000,976.0000,2000.0000,-0.9680,0.1280,0.0000,10.0879,0.0003,0.0000,0.6020,0.3160,0.0000,-0.4040,0.0000,0.0020,0.0000,1.0000,-0.6440,0.5200,0.0000,0.0000])

(def bip-snd  (.createWave js/jsfxlib bip))
(def first-bar-bip-snd  (.createWave js/jsfxlib first-bar-bip))


(defn calculate-ms [bpm]
  (* (/ 1 (/ bpm 60)) 1000))

(defn is-first-beat [beats upper-tempo]
    (= (mod beats upper-tempo) 0))

(defn note-duration [bpm note]
  (let [delay (delay-times (keyword (str note)))]
    (* 1000 (/ delay bpm))))



(defn toggle-sound [data]
  (let [interval-id (@data :interval-id)
        new-interval (note-duration (@data :bpm) (@data :beat-duration))
        playing (@data :playing)]
    (println "New interval is..." new-interval)
    (when interval-id
      (js/clearInterval interval-id))
    (when playing
      (om/transact! data
                  :interval-id
                  (fn []
                   (js/setInterval
                      (fn []
                        (om/transact! data :current-beat inc)
                        (let [curr-beat (@data :current-beat)
                              beats-bar (@data :beats-per-bar)
                              the-mod (mod curr-beat beats-bar)]

                          (println "----- INIT PLAY --------")

                          (println @data)
                          (println the-mod)
                          (condp = the-mod
                            0 (do
                                (.pause bip-snd)
                                (set! (.-currentTime first-bar-bip-snd) 0)
                                (.play first-bar-bip-snd))
                            1 (do
                                (.pause first-bar-bip-snd)
                                (set! (.-currentTime bip-snd) 0)
                                (.play bip-snd)
                                )
                            (do
                              (.pause first-bar-bip-snd)
                              (set! (.-currentTime bip-snd) 0)
                              (.play bip-snd))))
                        (println "------- END PLAY --------"))

                    new-interval)))
    (println "Data after " @data))))

(mod 29 4)
(defn display [flag]
  (if flag
    #js {}
    #js {:display "none"}))

(extend-type js/String
  ICloneable
  (-clone [s] (js/String. s))
  om/IValue
  (-value [s] (str s)))

(defn bpm-setter [data owner]
  (reify
    om/IRenderState
      (render-state [this state]
        (dom/div #js {:style (display (not (state :editing)))}
          (dom/button
              #js {:onClick
                    (fn [e]
                      (om/transact! data :bpm dec)
                      (toggle-sound data))}
             "-")
          (dom/button
              #js {:onClick
                    (fn [e]
                      (om/transact! data :bpm inc)
                      (toggle-sound data))}
             "+") ))))


(defn bpm-label [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:editing false})
    om/IRenderState
    (render-state [this {:keys [editing] :as state}]
       (dom/div nil
        (dom/label #js {:style (display (not editing))
                        :onClick (fn [_]
                                   (om/set-state! owner :editing (not editing)))
                        }
                   (str (om/value (:bpm data)) " BPM"))
        (dom/input #js {:style (display editing)
                        :ref "set-bpm"
                        :type "text"
                        :value (:bpm data)
                        :onKeyPress #(when (== (.-keyCode %) 13)
                                       (om/set-state! owner :editing false))
                        :onChange (fn [e]
                                    (let [value (.. e -target -value)]
                                      (when (> (js/parseInt value) 0)
                                        (om/transact! data :bpm (fn [_] (js/parseInt (.. e -target -value)))))))
                        :onBlur (fn [e] (let [value (.. e -target -value)]
                                          (om/set-state! owner :editing false)))})
        (om/build bpm-setter data {:state state})))))


(defn time-signature [data owner]
  (reify
    om/IInitState
    (init-state [this]
      {:lower 4 :upper 4})
    om/IRenderState
    (render-state [this {:keys [lower upper] :as state}]
      (dom/div nil
          (dom/label nil "Time Signature")
               (apply dom/select #js {:value (om/get-state owner :upper)
                                      :onChange (fn [e]
                                                  (let [value (.. e -target -value)]
                                                    (om/set-state! owner :upper (js/parseInt value))
                                                    (om/transact! data :beats-per-bar (fn[_] value))))}
                            (map (fn [k]
                               (dom/option #js {:value k} k))
                                   (range 1 13)))
               (dom/label nil "/")
               (apply dom/select #js {:value (om/get-state owner :lower)
                                      :onChange (fn [e]
                                                  (let [value (.. e -target -value)]
                                                    (om/set-state! owner :lower (js/parseInt value))
                                                    (om/transact! data :beat-duration (fn[_] value))))
                                      }
                            (map (fn [k]
                               (dom/option #js {:value (name k)} (name k)))
                                   (keys delay-times)))))))

(defn start-button [data owner]
  (reify

    om/IRender
    (render [_]
     (dom/div nil
      (dom/button
        #js {:onClick
               (fn [e]
                 (om/transact! data :playing not)
                 (toggle-sound data)
                 )}
                 "Start")))))

(defn stop-button [data owner]
  (reify

    om/IRender
    (render [_]
     (dom/div nil
      (dom/button
        #js {:onClick
               (fn [e]
                 (om/transact! data :playing not)
                 (om/transact! data :current-beat (fn [_] 0))
                 (toggle-sound data))}
                 "Stop")))))

(defn container [data owner]
  (reify
    om/IRender
    (render [_]
            (dom/div nil
                  (om/build bpm-label data)
                   (om/build time-signature data)
                     (if (data :playing)
                       (om/build stop-button data)
                       (om/build start-button data)
                       )))))
(om/root
  container
  app-state
  {:target (. js/document (getElementById "app"))})

