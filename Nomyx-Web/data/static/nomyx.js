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
function divBoxvisibility(idDiv, classDiv, idButton, classButton)
{
   //push back every divs
   var boxes = document.getElementsByClassName(classDiv);
   for (i = 0; i < boxes.length; i++) {
      boxes[i].style.display = 'none';
   }

   //pull front our div
   var myBox = document.getElementById(idDiv);
   myBox.style.display = 'inline';

   //unbold the text in the buttons
   var buttons = document.getElementsByClassName(classButton);
   for (i = 0; i < buttons.length; i++) {
      buttons[i].style.fontWeight = 'normal';
   }

   //pull front our div
   var myButton = document.getElementById(idButton);
   myButton.style.fontWeight = 'bold';

}

function setDivVisibility(groupName, elementName) {

   var idDiv       = elementName + "Div";
   var classDiv    = groupName   + "Div";
   var idButton    = elementName + "Button";
   var classButton = groupName   + "Button";

   divBoxvisibility(idDiv, classDiv, idButton, classButton);
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

