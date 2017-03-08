function toggle_visibility(id_events, id_show)
{
   var events = document.getElementById(id_events);
   var show = document.getElementById(id_show);
   if (show.innerHTML == 'Click to show')
   {
      events.style.display = 'block';
      show.innerHTML = 'Click to hide';
   }
   else
   {
      events.style.display = 'none';
      show.innerHTML ='Click to show';
   }
}

//toggle visibility for overlapping divs
//all elements with the class name will be pushed back,
//while only the element with the id name is put on front
function toggleVisibilityGroup(elementId, groupClass)
{
   var elems = document.getElementsByClassName(groupClass);
   for (i = 0; i < elems.length; i++) {
      elems[i].style.display = 'none';
   }

   var elem = document.getElementById(elementId);
   elem.style.display = 'inline';
}

function toggleBoldGroup(elementId, groupClass)
{
   var elems = document.getElementsByClassName(groupClass);
   for (i = 0; i < elems.length; i++) {
      elems[i].style.fontWeight = 'normal';
   }

   var elem = document.getElementById(elementId);
   elem.style.fontWeight = 'bold';
}


function setDivVisibilityAndSave(groupName, elementName) {

   setDivVisibility(groupName, elementName);
   setCookie("divVis" + groupName, elementName);

   console.log("saving div visibility: " + groupName + "=" + elementName);
}

function loadDivVisibility() {

   cookies = getCookies("divVis");

   console.log("loadDivVisibility:" + cookies);
   for(var i=0; i<cookies.length; i++) {
      element = cookies[i];
      group = cookies[i].split('-')[0];
      console.log("setting visibility: group = " + group + " element = " + element);
      setDivVisibility(group, element);

   }
}

function setCookie(cname, cvalue) {
    var d = new Date();
    d.setTime(d.getTime() + (365*24*60*60*1000));
    var expires = "expires="+d.toUTCString();
    document.cookie = cname + "=" + encodeURIComponent(cvalue) + "; " + expires;
}

function getCookies(cname) {

    var allCookies = document.cookie.split(';');
    console.log("getCookies:" + allCookies);
    var results = [];
    for(var i=0; i<allCookies.length; i++) {
        nameValue = allCookies[i].split('=');
        console.log("getCookies: nameValue[0]=" + nameValue[0] + " nameValue[1]=" + nameValue[1]);
        if (nameValue[0].search(cname) != -1) {
           results.push(nameValue[1]);
        }
    }
    return results;
}

function getQueryVariable(variable) {
    var query = window.location.search.substring(1);
    var vars = query.split('&');
    for (var i = 0; i < vars.length; i++) {
        var pair = vars[i].split('=');
        if (decodeURIComponent(pair[0]) == variable) {
            return decodeURIComponent(pair[1]);
        }
    }
    console.log('Query variable %s not found', variable);
}

$(document).ready(function () {
    templateQuery();
    ruleQuery();
    moduleQuery();
});

function templateQuery() {
    var ruleName = getQueryVariable("ruleName");
    var isRuleEdit = getQueryVariable("edit");

    $('[id="' + ruleName + '"]').css('display', 'block');
    if (isRuleEdit) {
       $('[id="' + ruleName + '"] .editRule').css('display', 'block');
    } else {
       $('[id="' + ruleName + '"] .viewRule').css('display', 'block');
    }
}

function ruleQuery() {
    var ruleNumber = getQueryVariable("ruleNumber");
    var decl = getQueryVariable("decl");

    $('[id="rule' + ruleNumber + '"]').css('display', 'block');
    
    if (decl) {
       $('[id="rule' + ruleNumber + '"] .ruleDecl').css('display', 'block');
    } else {
       $('[id="rule' + ruleNumber + '"] .ruleMain').css('display', 'block');
    }
}

function moduleQuery() {
    var modulePath = getQueryVariable("modulePath");
    $('[id="' + modulePath + '"]').css('display', 'block');
}
