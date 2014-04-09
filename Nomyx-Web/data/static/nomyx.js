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
