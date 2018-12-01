#!/bin/bash

# shellcheck disable=SC2154

source ./{{ ProjectName }}

oneTimeSetUp() { :; }
oneTimeTearDown() { :; }
setUp() { :; }
tearDown() { :; }

{% NORMAL %}

source shunit2
