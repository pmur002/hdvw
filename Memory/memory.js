
const state = (function() {
    let stimCol = 0;
    let matchCol = 0;

    function angleDiff(x, y) {
        var diff = Math.abs(x - y);
        if (diff > 180)
            diff = 360 - diff;
        return diff;
    }

    function visID(id, visibility) {
        const elt = document.getElementById(id);
        elt.style.visibility = visibility;
    }
    
    function showID(id) { visID(id, "visible"); }
    function hideID(id) { visID(id, "hidden"); }

    function visClass(cl, visibility) {
        const elts = document.getElementsByClassName(cl);
        for (let i = 0; i < elts.length; i++) {
            elts[i].style.visibility = visibility;
        }
    }

    function showClass(id) { visClass(id, "visible"); }
    function hideClass(id) { visClass(id, "hidden"); }

    return {

        phaseOne(x) {
            /* Initialisation */
            stimCol = 0;
            matchCol = 0;
            /* Show random stimulus */
            const stimuli = document.getElementsByClassName("stimulus");
            const n = stimuli.length;
            const which = Math.floor(Math.random()*n + 1) + 1;
            showID(stimuli[which].id);
            /* Hide match */
            hideID("mask.1");
            hideID("match.1");
            hideClass("matchMark");
            hideClass("stimMark");
            hideID("error.1");
            hideID("play-again.1");
        },
        
        phaseTwo(x) {
            stimCol = parseInt(x.id.replaceAll(/.+-|[.].+/g, ""));
            /* Hide stimuli */
            hideClass("stimulus");
            /* Show match */
            showID("mask.1");
            showID("match.1");
        },
        
        phaseThree(x) {
            matchCol = parseInt(x.id.replace(/.+[.]/, ""));
            error = angleDiff(matchCol, stimCol);
            /* Show stimulus mark */
            showID("stimMark-" + stimCol + ".1");
            /* Show match mark */
            showID("matchMark-" + matchCol + ".1");
            /* Show error */
            errorMsg = document.getElementById("error.1.1.tspan.1");
            errorMsg.textContent = "Error = " + error.toFixed(1);
            showID("error.1");
            /* Show play again */
            showID("play-again.1");
        }
    };
})();




