// If the current page contains an element with id="page-nav"
//  then this script will populate it with <li> elements
//  containing links to all the <h2> elements on the current page
if (!!document.getElementById("page-nav")){

    var headings = document.querySelectorAll("h2[id]");

    for (var i = 0; i < headings.length; i++) {
        document.getElementById("page-nav").innerHTML +=
            '<li id="nav-' + headings[i].id + 
                '"><a href="#' + headings[i].id + '">' +
                headings[i].innerText +
            '</a></li>';
    }

    $(document).ready(function() {
    $(window).scroll(function() {

        var found = false;
        var scrollPos = $(window).scrollTop();
        for (var i = 0; i < headings.length; i++) {

        if (scrollPos >= headings[i].offsetTop){
            found = true;
            $("#nav-"+headings[i].id).addClass('current');
        } else {
            $("#nav-"+headings[i].id).removeClass('current');
        }

        }

    });
    });

}