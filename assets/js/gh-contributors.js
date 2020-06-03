// 
if (!!document.getElementById("gh-contributors")){

    var topChart;

    var repoFilter = "all";

    var repos = ['fortran-lang-fortran-lang.org','fortran-lang-fpm','fortran-lang-stdlib','j3-fortran-fortran_proposals'];

    var repoData = repos.map( r => loadJSON('/community/github_stats_data/data-'+r+'.json') );

    var repoList = repoData.map(r=>r.name);

    setupInterface(repoList);

    var repoUserComments = new Map();
    
    for (var i = 0; i < repoData.length; i++){
        repoUserComments.set(repoData[i].name, getUserComments(repoData[i]));
    }
    
    var allUserComments = combineRepoData(repoData);
    repoUserComments.set('all',allUserComments);
    
    var dateBounds = getDateBounds(allUserComments);
    
    var tmin0 = dateBounds[0];
    var tmax0 = dateBounds[1];

    updateSliderDates(dateBounds);
    
    var oneday = Date.UTC(96, 1, 2) - Date.UTC(96, 1, 1);
    var bucketSize0 = 7*oneday;

    generateContributorStats();

    console.log("Done.")

}

if (!!document.getElementById("gh-contributors-slider")){

    setupSliderInterface();

    $(function() {
        $( "#slider-range" ).slider({
            range: true,
            min: tmin0 / 1000,
            max: tmax0 / 1000,
            step: 86400,
            values: [ tmin / 1000, tmax / 1000 ],
            slide: function( event, ui ) {
                $( "#amount" )[0].innerHTML = (new Date(ui.values[ 0 ] *1000).toDateString() ) + " - " + (new Date(ui.values[ 1 ] *1000)).toDateString() ;
            },
            stop: function( event, ui) {
                tmin = ui.values[ 0 ] *1000;
                tmax = ui.values[ 1 ] *1000;
                generateContributorStats();
            }
        });
        $( "#amount" )[0].innerHTML = (new Date($( "#slider-range" ).slider( "values", 0 )*1000).toDateString()) +
        " - " + (new Date($( "#slider-range" ).slider( "values", 1 )*1000)).toDateString() ;
    });

    plotAggregateData(allUserComments);

}

function repoSelector(selector){

    repoFilter = selector.value;

    var userComments = repoUserComments.get(repoFilter)
    
    // Update date slider for selected repo
    dateBounds = getDateBounds(userComments);
    updateSliderDates(dateBounds);
    
    plotAggregateData(userComments)
    
    // Update user charts
    generateContributorStats(true);

}

function updateSliderDates(dateBounds){

    if (!!document.getElementById("gh-contributors").hasAttribute("data-startdate")){
        tmin = new Date(document.getElementById("gh-contributors").getAttribute("data-startdate")).getTime();
    } else {
        tmin = dateBounds[0];
    }

    if (!!document.getElementById("gh-contributors").hasAttribute("data-enddate")){
        tmax = new Date(document.getElementById("gh-contributors").getAttribute("data-enddate")).getTime();
    } else {
        tmax = dateBounds[1];  
    }

    $( "#amount" )[0].innerHTML = (new Date(tmin).toDateString()) +
        " - " + (new Date(tmax).toDateString()) ;

    if (!!document.getElementById("gh-contributors-slider")){

        $("#slider-range").slider("values",0,tmin/1000);
        $("#slider-range").slider("values",1,tmax/1000);
    }

}


// Plot aggregate chart at top
//
function plotAggregateData(userComments){

    if (!!document.getElementById("gh-contributors-slider")){

        userNames = [...userComments.keys()];
        var allDates = [].concat(...userNames.map(u=>userComments.get(u)));
        plotData = getUserPlotData(allDates,tmin0,tmax0,bucketSize0)

        if (!!topChart){
            topChart.destroy();
        }
        topChart = plotContributorChart('all',plotData,true);
    }

}

function generateContributorStats(resetRange = false){

    var filteredComments = filterComments(repoUserComments.get(repoFilter),tmin,tmax);

    var userNames = [...filteredComments.keys()];
    
    var userSort = userNames.sort((u1,u2) => filteredComments.get(u2).length - filteredComments.get(u1).length);

    document.getElementById("gh-contributor-list").innerHTML = '';
    for (var i = 0; i < userSort.length; i++){ //userNames.length; i++){

        addContributorChart(userSort[i],filteredComments.get(userSort[i]).length,$('#userPlots')[0].checked);
    }

    var oneday = Date.UTC(96, 1, 2) - Date.UTC(96, 1, 1);
    var bucketSize = (tmax-tmin)/10;// 7*oneday;

    if ($('#userPlots')[0].checked){

        for (var i=0; i < userSort.length; i++){

            plotData = getUserPlotData(filteredComments.get(userSort[i]),tmin,tmax,bucketSize)
            plotContributorChart(userSort[i],plotData);

        }

    }

}

function getDateBounds(userCommentData){

    var userNames = [...userCommentData.keys()];

    var allDates = [].concat(...userNames.map(u=>userCommentData.get(u))).map(c => c.date.valueOf());

    var tmin = Math.min(...allDates);
    var tmax = Math.max(...allDates);

    return [tmin, tmax];

}

function filterComments(userComments,startDate=0,endDate=Infinity){

    userCommentData2 = new Map(userComments);

    userNames = [...userCommentData2.keys()];
    
    for (var i=0; i< userNames.length; i++){

        userData = userCommentData2.get(userNames[i]);

        userData = userData.filter(c => c.date.valueOf() >= startDate &&
                                        c.date.valueOf() <= endDate);
        
        if (userData.length > 0){
            userCommentData2.set(userNames[i],userData);
        } else {
            userCommentData2.delete(userNames[i]);
        }

    }

    return userCommentData2;
}

function getUserPlotData(userData,tmin,tmax,bucketSize){


    var start = Math.min( ...userData.map(c => c.date.valueOf()) );
    var end = Math.max( ...userData.map(c => c.date.valueOf()) );

    var bi = 0;
    var plotData = [];
    plotData.push({x: tmin-bucketSize, y: 0});
    plotData.push({x: start-bucketSize, y: 0});

    for (var d = start; d < end+bucketSize; d=d+bucketSize){

        count = userData.map(c => c.date>=d && c.date < (d+bucketSize)).filter(Boolean).length;

        plotData.push({x: new Date(d), y: count})

        bi = bi + 1;

    }
    plotData.push({x: end+bucketSize, y: 0});
    plotData.push({x: tmax+bucketSize, y: 0});

    return plotData;
}



function getUserComments(repo){

    var userCommentData = new Map();

    for (var i = 0; i < repo.issues.length; i++) {

        if (!userCommentData.has(repo.issues[i].user)){
            userCommentData.set(repo.issues[i].user,[]);
        }

        userData = userCommentData.get(repo.issues[i].user);
        userData.push({repo: repo.name, date: new Date(repo.issues[i].date)});

        let comments = repo.issues[i].comments;

        for (var j = 0; j < comments.length; j++) {

            if (comments[j].user.includes('[bot]')){
                continue;
            }

            if (!userCommentData.has(comments[j].user)){
                userCommentData.set(comments[j].user,[]);
            }

            userData = userCommentData.get(comments[j].user);
            userData.push({repo: repo.name, date: new Date(comments[j].date)});
            
        }

    }

    userCommentData = sortUserComments(userCommentData);

    return userCommentData;

}


function combineRepoData(repoData){

    allRepoData = {issues: [].concat(...repoData.map(r => r.issues))}

    return getUserComments(allRepoData);

}

function sortUserComments(userComments){

    userNames = [...userComments.keys()];

    for (var i = 0; i < userNames.length; i++){
        userData = userComments.get(userNames[i]);
        userData.sort((a,b) => a.date - b.date);
    }

    return userComments;
}


// Add a new contributor box
//  to #gh-contributors element
//
function addContributorChart(userName, nComment, plot=true){

    // document.getElementById("gh-contributors").innerHTML += 
    //      '<div class="col-flex"> <h3>'+
    //      '<a href="https://github.com/"'+userName+'>' userName+'</h3>'+
    //      '<canvas id="chart-'+userName+'" width="250" height="200"></canvas>'+
    //      '</div>';

    document.getElementById("gh-contributor-list").innerHTML += 
        Mustache.render(`
        <div class="col-flex contributor"> 
        <img src="https://github.com/{{userName}}.png?size=40" width="40" style="border-radius: 7px;">
        <div style="display: inline-block; vertical-align: middle; margin-left: 10px">
        <a class="hidenewwindow" href="https://github.com/{{userName}}" target="_blank" rel="noopener" style="font-size:22px;">
        {{userName}} </a> </br>
        <i><b> {{nComment}} comments </b></i>
        </div>
        {{#plot}}
        <div style="position: relative;">
        <canvas id="chart-{{userName}}"></canvas>
        </div>
        {{/plot}}
        </div>
        `,{userName: userName, nComment:nComment, plot: plot});

}

// Populate contributor chart 
//  with (x,y) plot data
//
function plotContributorChart(userName,plotData_i,responsive=false){

    ctx_i= document.getElementById('chart-'+userName).getContext('2d');

    charts_i = new Chart(ctx_i, {
        type: 'scatter',
        data: {
            datasets: [{
                label: 'Scatter Dataset',
                data: plotData_i
            }]
        },
        options: plotOptions(responsive)
    });

    return charts_i;
}


function setupInterface(repoList){

    console.log(repoList);
    
    var content = `
    <b>Repository: </b>
    <select id="repo-selector" onchange="repoSelector(this);">
    <option value="all">All</option>
    {{#repoList}}
        <option value="{{name}}" >{{name}}</option>
    {{/repoList}}
    </select>
    &emsp;
    <b>User plots: </b>
    <input type="checkbox" id="userPlots" value="1" onchange="generateContributorStats();">
    &emsp;
    <span id="amount" style="border: 0; color: #734f96; font-weight: bold;"></span>
    `;

    document.getElementById("gh-contributors").innerHTML = 
        Mustache.render(content,{repoList: repoList.map(r => {return{name:r}})});
    
    document.getElementById("gh-contributors").innerHTML += 
    `
    <div class="container-flex" id="gh-contributor-list"></div>
    `
}

function setupSliderInterface(){

    var content = `
    <canvas id="chart-all" height="75px"></canvas>
    <div id="slider-range" style="margin: 10px 30px;"></div>
    `;

    document.getElementById("gh-contributors-slider").innerHTML = content;

}

// Configuration for chart.js
//  https://www.chartjs.org/docs/latest/
//
function plotOptions(responsive=false) {
    return {
        responsive: true,
        aspectRatio: 2,
        legend: {
            display: false
        },
        elements: {
            point: {
                radius: 0
            },
            line: {
                backgroundColor: 'rgba(115, 79, 150, 0.5)',
                cubicInterpolationMode: 'monotone'
            }
        },
        scales: {
            xAxes: [{
                type: 'time',
                time: {
                    unit: 'month'
                },
                position: 'bottom'
            }],
            yAxes: [{
                ticks:{
                    // max: 100
                    maxTicksLimit: 5
                }
            }]
        },
        datasets: {
            scatter: {
                showLine: true
            }
        }
    }
}


function loadJSON(url) {
    var json = null;
    $.ajax({
        'async': false,
        'global': false,
        'url': url,
        'dataType': "json",
        'success': function (data) {
            json = data;
        }
    });
    return json;
}