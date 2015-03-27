(ns fzy-cljr.core-test
  (:require [clojure.test :refer :all]
            [fzy-cljr.core :refer :all]))

(deftest test-palindrome-true
	(is (= true (palindrome? "bob"))))

(deftest test-palindrome-false
	(is (= false (palindrome? "bro"))))

(deftest test-levenshtein-1
	(is (= 8 (levenshtein "rosettacode" "raisethysword"))))