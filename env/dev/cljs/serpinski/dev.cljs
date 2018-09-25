(ns ^:figwheel-no-load serpinski.dev
  (:require
    [serpinski.core :as core]
    [devtools.core :as devtools]))

(devtools/install!)

(enable-console-print!)

(core/init!)
