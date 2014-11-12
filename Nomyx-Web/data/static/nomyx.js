function toggle_visibility(id_events, id_show) 
{ 
   var events = document.getElementById(id_events);
   var show = document.getElementById(id_show);
   if (show.innerHTML == '[Click to show]') 
   {
      events.style.display = 'block'; 
      show.innerHTML = '[Click to hide]';
   }
   else   
   {
      events.style.display = 'none'; 
      show.innerHTML ='[Click to show]';
   }
}

//toggle visibility for overlapping divs
//all elements with the class name will be pushed back, 
//while only the element with the id name is put on front
function div_visibility(id_name, class_name)
{
   //push back every divs
   var divs = document.getElementsByClassName(class_name);
   for (i = 0; i < divs.length; i++) {
      divs[i].style.display = 'none';
   }

   //pull front our div
   var myDiv = document.getElementById(id_name);
   myDiv.style.display = 'inline';

}
