// ==UserScript==
// @name         Hide reddit username
// @namespace    http://tampermonkey.net/
// @version      0.1
// @description  Hide my reddit username when logged in
// @author       Me
// @match        https://*.reddit.com/*
// @grant        GM_addStyle
// ==/UserScript==

// This is a userscript for tampermonkey

(function() {
    'use strict';
    document.querySelector('span._2BMnTatQ5gjKGK5OWROgaG').style.display='none'; // Hide username
    document.querySelector('span._1pHyKCBktIf_9WFW9jjM3P').style.display='none'; // Hide my karma
    document.querySelector('div._2kZkQ13N-kvhDEJOBd1S1I').style.display='none'; // Hide my username in the comment section
})();
