// ==UserScript==
// @name         Hide reddit username
// @namespace    http://tampermonkey.net/
// @version      0.1
// @description  Hide my reddit username when logged in
// @author       Me
// @match        https://*.reddit.com/*
// @grant        none
// ==/UserScript==

// This is a userscript for tampermonkey

(function() {
    'use strict';
    document.querySelector('span._2BMnTatQ5gjKGK5OWROgaG').style.display='none';
})();
