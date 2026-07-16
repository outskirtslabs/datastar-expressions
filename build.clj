;; Copyright © 2025 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: MIT
(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def project (-> (edn/read-string (slurp "deps.edn")) :aliases :neil :project))
(def lib (:name project))
(def version (:version project))
(def license-id (-> project :license :id))
(def license-file (or (-> project :license :file) "LICENSE"))
(def description (:description project))

(defn- git-origin-url []
  (try
    (some-> (b/git-process {:git-args "remote get-url origin"})
            str/trim
            (str/replace #"\.git$" ""))
    (catch Exception _
      nil)))

(defn- git-rev []
  (or (some-> (System/getenv "GIT_REV")
              str/trim
              not-empty)
      (some-> (b/git-process {:git-args "rev-parse HEAD"})
              str/trim
              not-empty)))

(def rev (git-rev))
(def repo-url-prefix (or (:url project) (git-origin-url)))
(assert lib ":name must be set in deps.edn under the :neil alias")
(assert version ":version must be set in deps.edn under the :neil alias")
(assert description ":description must be set in deps.edn under the :neil alias")
(assert license-id "[:license :id] must be set in deps.edn under the :neil alias")
(assert rev "Either GIT_REV must be set or git rev-parse HEAD must succeed")
(assert repo-url-prefix "Either :url must be set in deps.edn under the :neil alias or git remote origin must exist")
(def class-dir "target/classes")
(def basis_ (delay (b/create-basis {:project "deps.edn"})))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn- existing-paths [paths]
  (->> paths
       (filter #(.exists (io/file %)))
       vec))

(defn permalink [subpath]
  (str repo-url-prefix "/blob/" rev "/" subpath))

(defn url->scm [url-string]
  (let [[_ domain repo-path] (re-find #"https?://?([\w\-\.]+)/(.+)" url-string)]
    [:scm
     [:url (str "https://" domain "/" repo-path)]
     [:connection (str "scm:git:https://" domain "/" repo-path)]
     [:developerConnection (str "scm:git:ssh:git@" domain ":" repo-path)]]))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (b/write-pom {:class-dir class-dir
                :lib       lib
                :version   version
                :basis     @basis_
                :src-dirs  (existing-paths ["src"])
                :pom-data  [[:description description]
                            [:url repo-url-prefix]
                            [:licenses
                             [:license
                              [:name license-id]
                              [:url (permalink license-file)]]]
                            (conj (url->scm repo-url-prefix) [:tag rev])]})

  (b/copy-dir {:src-dirs   (existing-paths ["src" "resources"])
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file  jar-file}))

(defn install [_]
  (jar {})
  (b/install {:basis     @basis_
              :lib       lib
              :version   version
              :jar-file  jar-file
              :class-dir class-dir}))

(defn deploy [opts]
  (jar opts)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
   (merge {:installer :remote
           :artifact  jar-file
           :pom-file  (b/pom-path {:lib lib :class-dir class-dir})}
          opts))
  opts)
