package com.mosesn.fql

// TODO add groupby?
// http://www.sqlite.org/lang_select.html
case class Query(select: WhatWill, when: When, where: Where)
