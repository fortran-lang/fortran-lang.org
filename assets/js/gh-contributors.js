ghContributorStats = function() {
  
    var oneday = Date.UTC(96, 1, 2) - Date.UTC(96, 1, 1);
    var topChart;
    var repoFilter = "all";
    var tmin;
    var tmax;

    if (!!document.getElementById("gh-contributors")){

        // Load JSON data
        var repoFiles = ['j3-fortran-fortran_proposals','fortran-lang-stdlib','fortran-lang-fpm','fortran-lang-fortran-lang.org'];
        var repoData = repoFiles.map( r => loadJSON('/community/github_stats_data/data-'+r+'.json') );
    
        // Preprocess data
        var repoUserContribs = new Map();    
        for (var i = 0; i < repoData.length; i++){
            repoUserContribs.set(repoData[i].name, getUserContribs(repoData[i]));
        }
        
        // Get combined dataset across all repos
        var allRepoData = {issues: [].concat(...repoData.map(r => r.issues))}
        var allUserContribs = getUserContribs(allRepoData);
        repoUserContribs.set('all',allUserContribs);
        
        // Setup HTML for interface
        var repoList = repoData.map(r=>r.name);
        makeContributorInterface(repoList);
    
        var globalDateBounds = getDateBounds(allUserContribs);
        resetDates(globalDateBounds);
    
        generateContributorStats();
    
        if (!!document.getElementById("gh-contributors-slider")){
        
            makeSliderInterface();
        
            plotAggregateData(allUserContribs);
        
        }
    }


    // Called by repository dropdown box
    //  
    function onRepoSelect(selector){

        repoFilter = selector.value;

        var userContribs = repoUserContribs.get(repoFilter)
        
        // Update date slider for selected repo
        dateBounds = getDateBounds(userContribs);
        resetDates(dateBounds);
        
        plotAggregateData(userContribs)
        
        // Update user charts
        generateContributorStats(true);

    }

    // Set date bounds to either data-startdate/data-enddate if specified
    //  or the min/max dates of the selected repo otherwise
    function resetDates(dateBounds){

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
    function plotAggregateData(userContribs){

        if (!!document.getElementById("gh-contributors-slider")){

            var bucketSize0 = 7*oneday;

            var userNames = [...userContribs.keys()];
            var allDates = [].concat(...userNames.map(u=>userContribs.get(u)));
            var plotData = getUserPlotData(allDates,globalDateBounds[0],globalDateBounds[1],bucketSize0)

            if (!!topChart){
                topChart.destroy();
            }
            topChart = plotContributorChart('all',plotData,timeUnit='month');
        }

    }


    // Regenerate the individual contributor data boxes
    //  
    function generateContributorStats(resetRange = false){

        var filteredContribs = filterContribs(repoUserContribs.get(repoFilter),tmin,tmax);

        var userNames = [...filteredContribs.keys()];
        
        var userSort = userNames.sort((u1,u2) => filteredContribs.get(u2).length - filteredContribs.get(u1).length);

        document.getElementById("gh-contributor-list").innerHTML = '';
        for (var i = 0; i < userSort.length; i++){

            addContributorChart(userSort[i],filteredContribs.get(userSort[i]).length,$('#userPlots')[0].checked);
        }

        var bucketSize = (tmax-tmin)/10;

        if ((tmax-tmin) < 10*7*oneday){
            var timeUnit = 'week';
        } else {
            var timeUnit = 'month';
        }

        if ($('#userPlots')[0].checked){

            for (var i=0; i < userSort.length; i++){

                var plotData = getUserPlotData(filteredContribs.get(userSort[i]),tmin,tmax,bucketSize)
                plotContributorChart(userSort[i],plotData,timeUnit);

            }

        }

    }


    // Get [min,max] date from userContrib data
    //
    function getDateBounds(userContribData){

        var userNames = [...userContribData.keys()];

        var allDates = [].concat(...userNames.map(u=>userContribData.get(u))).map(c => c.date.valueOf());

        var tmin = Math.min(...allDates);
        var tmax = Math.max(...allDates);

        return [tmin, tmax];

    }


    // Filter user contribution data based on date
    //
    function filterContribs(userContribs,startDate=0,endDate=Infinity){

        var userContribData2 = new Map(userContribs);

        var userNames = [...userContribData2.keys()];
        
        for (var i=0; i< userNames.length; i++){

            var userData = userContribData2.get(userNames[i]);

            userData = userData.filter(c => c.date.valueOf() >= startDate &&
                                            c.date.valueOf() <= endDate);
            
            if (userData.length > 0){
                userContribData2.set(userNames[i],userData);
            } else {
                userContribData2.delete(userNames[i]);
            }

        }

        return userContribData2;
    }


    /// Generate histogram plot data for a single user
    //
    function getUserPlotData(userData,tmin,tmax,bucketSize){


        var start = Math.min( ...userData.map(c => c.date.valueOf()) );
        var end = Math.max( ...userData.map(c => c.date.valueOf()) );

        var bi = 0;
        var plotData = [];
        plotData.push({x: tmin, y: 0});
        plotData.push({x: start-bucketSize, y: 0});

        for (var d = start; d < end+1; d=d+bucketSize){

            count = userData.map(c => c.date>=d && c.date < Math.min(d+bucketSize,end+1) ).filter(Boolean).length;

            plotData.push({x: new Date(d+0.5*bucketSize), y: count})

            bi = bi + 1;

        }
        plotData.push({x: end+bucketSize, y: 0});
        plotData.push({x: tmax, y: 0});

        return plotData;
    }


    // Preprocess json data to get
    //  username => [contributions] map
    //
    function getUserContribs(repo){

        var userContribData = new Map();

        for (var i = 0; i < repo.issues.length; i++) {

            if (!userContribData.has(repo.issues[i].user)){
                userContribData.set(repo.issues[i].user,[]);
            }

            var userData = userContribData.get(repo.issues[i].user);
            userData.push({repo: repo.name, date: new Date(repo.issues[i].date)});

            let comments = repo.issues[i].comments;

            for (var j = 0; j < comments.length; j++) {

                if (comments[j].user.includes('[bot]')){
                    continue;
                }

                if (!userContribData.has(comments[j].user)){
                    userContribData.set(comments[j].user,[]);
                }

                userData = userContribData.get(comments[j].user);
                userData.push({repo: repo.name, date: new Date(comments[j].date)});
                
            }

        }

        // Sort user contributions by date
        userNames = [...userContribData.keys()];
        for (var i = 0; i < userNames.length; i++){
            userData = userContribData.get(userNames[i]);
            userData.sort((a,b) => a.date - b.date);
        }

        return userContribData;

    }


    // Add a new contributor box
    //  to #gh-contributors-list element
    //
    function addContributorChart(userName, nContrib, plot=true){

        document.getElementById("gh-contributor-list").innerHTML += 
            Mustache.render(`
            <div class="col-flex contributor"> 
            <div style="display: inline-block; min-width: max-content;">
            <img src="https://github.com/{{userName}}.png?size=40" width="40" style="border-radius: 7px;">
            <div style="display: inline-block; vertical-align: middle; margin-left: 10px;">
            <a class="hidenewwindow" href="https://github.com/{{userName}}" target="_blank" rel="noopener" style="font-size:22px;">
            {{userName}} </a> </br>
            <i><b> {{nContrib}} contributions </b></i>
            </div>
            </div>
            {{#plot}}
            <div style="position: relative;">
            <canvas id="chart-{{userName}}"></canvas>
            </div>
            {{/plot}}
            </div>
            `,{userName: userName, nContrib:nContrib, plot: plot});

    }


    // Populate contributor chart 
    //  with (x,y) plot data
    //
    function plotContributorChart(userName,plotData, timeUnit){

        var ctx= document.getElementById('chart-'+userName).getContext('2d');

        var chart = new Chart(ctx, {
            type: 'scatter',
            data: {
                datasets: [{
                    label: 'Scatter Dataset',
                    data: plotData
                }]
            },
            options: plotOptions(timeUnit)
        });

        return chart;
    }


    // Configuration for chart.js
    //  https://www.chartjs.org/docs/latest/
    //
    function plotOptions(timeUnit) {

        if (timeUnit == 'week'){
            var minRotation = 10;
        } else {
            var minRotation = 0;
        }

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
                        unit: timeUnit
                    },
                    position: 'bottom',
                    ticks:{
                        minRotation: minRotation
                    }
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


    // Add HTML to 'gh-contributors' element for interface
    //
    function makeContributorInterface(repoList){
        
        var content = `
        <b>Repository: </b>
        <select id="repo-selector" onchange="ghContributorStats.onRepoSelect(this);">
        <option value="all">All</option>
        {{#repoList}}
            <option value="{{name}}" >{{name}}</option>
        {{/repoList}}
        </select>
        &emsp;
        <b>User plots: </b>
        <input type="checkbox" id="userPlots" value="1" onchange="ghContributorStats.generateContributorStats();">
        &emsp;
        <span id="amount" style="border: 0; color: #734f96; font-weight: bold;"></span>
        `;

        document.getElementById("gh-contributors").innerHTML = 
            Mustache.render(content,{repoList: repoList.map(r => {return{name:r}})});
        
        if (document.getElementById("gh-contributors").hasAttribute("height")){
            document.getElementById("gh-contributors").innerHTML += 
            Mustache.render(
            `
            <div style="overflow-x: auto; height: {{height}}"
            class="container-flex" id="gh-contributor-list"></div>
            `,{height: document.getElementById("gh-contributors").getAttribute("height")});
        } else {
            document.getElementById("gh-contributors").innerHTML += 
        `
        <div class="container-flex" id="gh-contributor-list"></div>
        `
        }
        
    }


    // Add HTML to 'gh-contributors-slider'
    //  for aggregate graph with date slider
    //
    function makeSliderInterface(){

        var content = `
        <canvas id="chart-all" height="75px"></canvas>
        <div id="slider-range" style="margin: 10px 30px;"></div>
        `;

        document.getElementById("gh-contributors-slider").innerHTML = content;

        $(function() {
            $( "#slider-range" ).slider({
                range: true,
                min: globalDateBounds[0] / 1000,
                max: globalDateBounds[1] / 1000,
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

    }


    // Load JSON data from url
    //
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


    return{
        onRepoSelect: onRepoSelect,
        generateContributorStats: generateContributorStats
    }

}();