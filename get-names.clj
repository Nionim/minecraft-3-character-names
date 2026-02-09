#!/usr/bin/env clojure -M
(require 
    '[clojure.data.json :as json]
    '[clojure.string :as str]
    '[clojure.set :as sets])
(import java.net.URL)

;; --==[ Vars ]==--
;; >> Name Charters
;;      - Why not in one line?
;;      - Reaaaadeble....
(def characters ["0123456789" "abcdefghijklmnopqrstuvwxyz" "_"])
;; >> Api url (U can replace it lul)
;;      >> Available names
(def apiAvailableURL "https://api.mojang.com/profiles/minecraft")
;;      >> Available soon names
(def apiAvailableSoonURL "https://api.mojang.com/users/profiles/minecraft/")
;; >> Dir for names
(def dirForNames "./")
;; >> Dir for names without numbers
(def dirWithoutNumbers "./with-out-numbers/")
;; >> Save file (Available names)
(def availableFile "available.txt")
;; >> Save file (Available soon names)
(def availableSoonFile "available-soon.txt")


;; --==[ Timeout vars ]==--
;; >> Timeout time
(def timeoutTime 30000)
;; >> Sleeping time (If response is not 200)
(def sleepingTime 60000)
;; >> Timeout time
(def rateLimit 1000)
;; >> Sleeping time (If response is not 200)
(def rateLimit2 500)

(def all-chars (apply str characters))
(def possible-names 
    (for [a all-chars b all-chars c all-chars] 
    (str a b c))
)

;; --==[ Timer ]==--
(defn sleep-ms [ms]
    (Thread/sleep ms))

;; --==[ Request ]==--
(defn api-request [method body api]
    (let [conn (.openConnection (URL. api))]
        (doto conn
            (.setRequestMethod method)
            (.setDoOutput (not (nil? body)))
            (.setConnectTimeout timeoutTime)
            (.setReadTimeout timeoutTime))
        (when body
            (.setRequestProperty conn "Content-Type" "application/json")
            (with-open [out (.getOutputStream conn)]
                (.write (.getBytes body))))
        conn))

(defn read-response [conn]
  (let [status (.getResponseCode conn)]
    (if (= status 200)
      (let [stream (.getInputStream conn)
            content (slurp stream)]
        (.close stream)
        content)
      (throw (Exception. (str "HTTP Error: " status))))))

;; --==[ Parse available names ]==--
(defn parse-available-names [names-batch]
    (try
        (let [conn (api-request "POST" (json/write-str names-batch) apiAvailableURL)
              response (read-response conn)]
            (map :name (json/read-str response :key-fn keyword)))
        (catch Exception ignored (sleep-ms sleepingTime) [])))

;; --==[ Parse available-soon names ]==--
(defn parse-available-soon-name [name]
    (try
        (let [conn (api-request "GET" nil (str apiAvailableSoonURL name))] (read-response conn) nil)
        (catch Exception e 
            (if (str/includes? (.getMessage e) "HTTP Error: 204")
                name (do (sleep-ms sleepingTime) nil)))))

;; --==[ Save to files ]==--
(defn ensure-dir [dir]
    (let [dir-file (java.io.File. dir)]
        (when-not (.exists dir-file)
            (.mkdirs dir-file))))

(defn save-to-file [names filename]
    (when (seq names)
        (with-open [writer (java.io.FileWriter. filename)]
            (doseq [name names]
                (.write writer (str name "\n"))))))

(defn save-all-files [available-names available-soon-names]
    (ensure-dir dirForNames)
    (ensure-dir dirWithoutNumbers)
    
    (save-to-file available-names (str dirForNames availableFile))
    (save-to-file available-soon-names (str dirForNames availableSoonFile))
    
    (let [without-numbers? (fn [name] (not-any? #(Character/isDigit %) name))
          available-wo-numbers (filter without-numbers? available-names)
          soon-wo-numbers (filter without-numbers? available-soon-names)]
        (save-to-file available-wo-numbers (str dirWithoutNumbers availableFile))
        (save-to-file soon-wo-numbers (str dirWithoutNumbers availableSoonFile))
    )
)

;; --==[ Main ]==--
(defn -main []    
    (let [batches (partition-all 100 possible-names)
          all-available (atom [])
          all-available-soon (atom [])
          total-batches (count batches)]
        
        (doseq [[idx batch] (map-indexed vector batches)]
            (let [available (parse-available-names batch)]
                (swap! all-available concat available)
                (Thread/sleep rateLimit)))
        
        ;; Check remaining names for available-soon status
        (let [checked-names (set @all-available)
              remaining-names (remove checked-names possible-names)]
            (doseq [[idx name] (map-indexed vector remaining-names)]
                (when (zero? (mod idx 100))
                (when-let [soon-name (parse-available-soon-name name)]
                    (swap! all-available-soon conj soon-name))
                (Thread/sleep rateLimit2)))
        (save-all-files (sort @all-available) (sort @all-available-soon)))
    )
)

;; --==[ The End lol ]==--
(if (= *file* (System/getProperty "babashka.file"))
    (-main))
