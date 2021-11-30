

$(document).ready(function() {

    var titleText = '';
    var currentTopicIndex = -1;
    var docProgressiveReveal = false;
    var docAllowSkip = false;
    var topics = [];

    var scrollLastSectionToView = false;
    var scrollLastSectionPosition = 0;

    function setCurrentTopic(topicIndex) {
      if (topics.length === 0) return;

      topicIndex = topicIndex * 1;  // convert strings to a number

      if (topicIndex == currentTopicIndex) return;

      if (currentTopicIndex != -1) {
        var el = $(topics[currentTopicIndex].jqElement);
        el.trigger('hide');
        el.removeClass('current');
        el.trigger('hidden');
        $(topics[currentTopicIndex].jqListElement).removeClass('current');
      }

      var currentEl = $(topics[topicIndex].jqElement);
      currentEl.trigger('show');
      currentEl.addClass('current');
      currentEl.trigger('shown');
      $(topics[topicIndex].jqListElement).addClass('current');
      currentTopicIndex = topicIndex;

      // always start a topic with a the scroll pos at the top
      // we do this in part to prevent the scroll to view behavior of hash navigation
      setTimeout(function() {$(document).scrollTop(0);}, 0);
    }

    function updateLocation(topicIndex) {
      var baseUrl = window.location.href.replace(window.location.hash,"");
      var href = baseUrl + '#' + topics[topicIndex].id;
      window.location = href;
    }

    function handleTopicClick(event) {
      hideFloatingTopics();
      updateLocation(this.getAttribute('index'));
    }

    function showFloatingTopics() {
      $('.topicsList').removeClass('hideFloating');
    }

    function hideFloatingTopics() {
      $('.topicsList').addClass('hideFloating');
    }

    function updateVisibilityOfTopicElements(topicIndex) {
      var topic = topics[topicIndex];

      if (!topic.progressiveReveal) return;

      var showSection = true;

      var lastVisibleSection = null;

      for (i = 0; i < topic.sections.length; i++ ) {
        var section = topic.sections[i];
        var sectionEl = $(section.jqElement);
        if (showSection) {
          sectionEl.trigger('show');
          sectionEl.removeClass('hide');
          sectionEl.trigger('shown');
          if (section.skipped) {
            sectionEl.removeClass('showSkip');
          }
          else {
            sectionEl.addClass('showSkip');
            lastVisibleSection = sectionEl;
          }
        }
        else {
          sectionEl.trigger('hide');
          sectionEl.addClass('hide');
          sectionEl.trigger('hidden');
        }
        showSection = (showSection && section.skipped);
      }

      if (!topic.progressiveReveal || showSection) { // all sections are visible
        $(topic.jqElement).removeClass('hideActions');
      }
      else {
        $(topic.jqElement).addClass('hideActions');
      }

      if (scrollLastSectionToView && lastVisibleSection) {
        scrollLastSectionPosition = lastVisibleSection.offset().top - 28;
        setTimeout(function() {
          $('html, body').animate({
            scrollTop: scrollLastSectionPosition
          }, 300);
        }, 60)
      }
      scrollLastSectionToView = false;
    }

    function updateTopicProgressBar(topicIndex) {
      var topic = topics[topicIndex];

      var percentToDo;
      if (topic.sections.length == 0) {
        percentToDo = !topic.topicCompleted * 100;
      }
      else {
        percentToDo = (1 - topic.sectionsSkipped/topic.sections.length) * 100;
      }

      $(topic.jqListElement).css('background-position-y', percentToDo + '%' );

    }

    function handleSkipClick(event) {
      var sectionId = this.getAttribute('data-section-id');
      // get the topic & section indexes
      var topicIndex = -1;
      var sectionIndex = -1;
      var topic;
      var section;
      $.each(topics, function( ti, t) {
        $.each(t.sections, function( si, s) {
          if (sectionId == s.id) {
            topicIndex = ti;
            sectionIndex = si;
            topic = t;
            section = s;
            return false;
          }
        })
        return topicIndex == -1;
      })
      // if the section has exercises and is not complete, don't skip - put up message
      if (section.exercises.length && !section.completed && !section.allowSkip) {
        var exs = section.exercises.length == 1 ? 'exercise' : 'exercises';
        bootbox.alert("You must complete the " + exs + " in this section before continuing.");
      }
      else {
        if (sectionIndex == topic.sections.length - 1) {
          // last section on the page
          if (topicIndex < topics.length - 1) {
            updateLocation(currentTopicIndex + 1);
          }
        }
        else {
          scrollLastSectionToView = true;
        }
        // update UI
        sectionSkipped([section.jqElement]);
        // notify server
        tutorial.skipSection(sectionId);
      }
    }

    function handleNextTopicClick(event) {
      // any sections in this topic? if not, mark it as skipped
      if (topics[currentTopicIndex].sections.length == 0) {
        tutorial.skipSection(topics[currentTopicIndex].id);
      }
      updateLocation(currentTopicIndex + 1);
    }

    function handlePreviousTopicClick(event) {
      updateLocation(currentTopicIndex - 1);
    }

    // build the list of topics in the document
    // and create/adorn the DOM for them as needed
    function buildTopicsList() {
      var topicsList = $('<div class="topicsList hideFloating"></div>');

      var topicsHeader = $('<div class="topicsHeader"></div>');
      topicsHeader.append($('<h2 class="tutorialTitle">' + titleText + '</h2>'));
      var topicsCloser = $('<div class="paneCloser"></div>');
      topicsCloser.on('click', hideFloatingTopics);
      topicsHeader.append(topicsCloser);
      topicsList.append(topicsHeader);

      $('#doc-metadata').appendTo(topicsList);


      var topicsDOM = $('.section.level2');
      topicsDOM.each( function(topicIndex, topicElement) {

        var topic = {};
        topic.id = $(topicElement).attr('id');
        topic.exercisesCompleted = 0;
        topic.sectionsCompleted = 0;
        topic.sectionsSkipped = 0;
        topic.topicCompleted = false; // only relevant if topic has 0 exercises
        topic.jqElement = topicElement;
        topic.jqTitleElement = $(topicElement).children('h2')[0];
        topic.titleText = topic.jqTitleElement.innerText;
        var progressiveAttr = $(topicElement).attr('data-progressive');
        if (typeof progressiveAttr !== typeof undefined && progressiveAttr !== false) {
          topic.progressiveReveal = (progressiveAttr == 'true' || progressiveAttr == 'TRUE');
        }
        else {
          topic.progressiveReveal = docProgressiveReveal;
        }

        jqTopic = $('<div class="topic" index="' + topicIndex + '">' + topic.titleText + '</div>');
        jqTopic.on('click', handleTopicClick);
        topic.jqListElement = jqTopic;
        $(topicsList).append(jqTopic);

        var topicActions = $('<div class="topicActions"></div>');
        if (topicIndex > 0) {
          var prevButton = $('<button class="btn btn-default">Previous Topic</button>');
          prevButton.on('click', handlePreviousTopicClick);
          topicActions.append(prevButton);
        }
        if (topicIndex < topicsDOM.length - 1) {
          var nextButton = $('<button class="btn btn-primary">Next Topic</button>');
          nextButton.on('click', handleNextTopicClick);
          topicActions.append(nextButton);
        }
        $(topicElement).append(topicActions);

        topic.sections = [];
        var sectionsDOM = $(topicElement).children('.section.level3');
        sectionsDOM.each( function( sectionIndex, sectionElement) {

          if (topic.progressiveReveal) {
            var continueButton = $('<button class="btn btn-default skip" data-section-id="' + sectionElement.id + '">Continue</button>');
            continueButton.on('click', handleSkipClick);
            var actions = $('<div class="exerciseActions"></div>');
            actions.append(continueButton);
            $(sectionElement).append(actions);
          }

          var section = {};
          section.exercises = [];
          var exercisesDOM = $(sectionElement).children('.tutorial-exercise');
          exercisesDOM.each(function(exerciseIndex, exerciseElement) {
            var exercise = {};
            exercise.dataLabel = $(exerciseElement).attr('data-label');
            exercise.completed = false;
            exercise.jqElement = exerciseElement;
            section.exercises.push(exercise);
          });

          var allowSkipAttr = $(sectionElement).attr('data-allow-skip');
          var sectionAllowSkip = docAllowSkip;
          if (typeof allowSkipAttr !== typeof undefined && allowSkipAttr !== false) {
            sectionAllowSkip = (allowSkipAttr == 'true' || allowSkipAttr == 'TRUE');
          }

          section.id = sectionElement.id;
          section.completed = false;
          section.allowSkip = sectionAllowSkip;
          section.skipped = false;
          section.jqElement = sectionElement;
          topic.sections.push(section);

        });

        topics.push(topic);
      });

      var topicsFooter = $('<div class="topicsFooter"></div>');

      var resetButton = $('<span class="resetButton">Start Over</span>');
      resetButton.on('click', function() {
        bootbox.confirm("Are you sure you want to start over? (all exercise progress will be reset)",
                        function(result) {
                          if (result)
                            tutorial.startOver();
                        });
      });
      topicsFooter.append(resetButton);
      topicsList.append(topicsFooter);

      return topicsList;

    }

    // transform the DOM here
  function transformDOM() {

    titleText = $('title')[0].innerText;

    var progAttr = $('meta[name=progressive]').attr("content");
    docProgressiveReveal = (progAttr == 'true' || progAttr == 'TRUE');
    var allowSkipAttr = $('meta[name=allow-skip]').attr("content");
    docAllowSkip = (allowSkipAttr == 'true' || allowSkipAttr == 'TRUE');

    var tutorialTitle = $('<h2 class="tutorialTitle">' + titleText + '</h2>');
    tutorialTitle.on('click', showFloatingTopics);
    $('.topics').prepend(tutorialTitle);

    $('.bandContent.topicsListContainer').append(buildTopicsList());

    // initialize visibility of all topics' elements
    for (var t = 0; t < topics.length; t++) {
      updateVisibilityOfTopicElements(t);
    }

    function handleResize() {
      $('.topicsList').css("max-height", window.innerHeight);
    }

    handleResize();
    window.addEventListener("resize", handleResize);

  }

  // support bookmarking of topics
  function handleLocationHash() {

    function findTopicIndexFromHash() {
      var hash = window.decodeURIComponent(window.location.hash);
      var topicIndex = 0;
      if (hash.length > 0) {
        $.each(topics, function( ti, t) {
          if ('#' + t.id == hash) {
            topicIndex = ti;
            return false;
          }
        });
      }
      return topicIndex;
    }

    // select topic from hash on the url
    setCurrentTopic(findTopicIndexFromHash());

    // navigate to a topic when the history changes
    window.addEventListener("popstate", function(e) {
      setCurrentTopic(findTopicIndexFromHash());
    });

  }

  // update UI after a section gets completed
  // it might be an exercise or it might be an entire topic
  function sectionCompleted(section) {
    var jqSection = $(section);

    var topicCompleted = jqSection.hasClass('level2');

    var topicId;
    if (topicCompleted) {
      topicId = jqSection.attr('id');
    }
    else {
      topicId = $(jqSection.parents('.section.level2')).attr('id');
    }

    // find the topic in our topics array
    var topicIndex = -1;
    $.each(topics, function(ti, t) {
      if (t.id == topicId) {
        topicIndex = ti;
        return false;
      }
    });
    if (topicIndex == -1) {
      console.log('topic "' + topicId + '" not found');
      return;
    }

    var topic = topics[topicIndex];

    if (topicCompleted) {         // topic completed
      topic.topicCompleted = true;
      updateTopicProgressBar(topicIndex);
    }
    else {                        // exercise completed
      var sectionIndex = -1;
      var sectionId = jqSection.attr('id');
      $.each(topic.sections, function(si, s ) {
        if (s.id == sectionId) {
          sectionIndex = si;
          return false;
        }
      })
      if (sectionIndex == -1) {
        console.log('completed section"' + sectionId + '"not found');
        return;
      }

      // update the UI if the section isn't already marked completed
      var section = topic.sections[sectionIndex];
      if (!section.completed) {
        topic.sectionsCompleted++;

        updateTopicProgressBar(topicIndex);

        // update the exercise
        $(section.jqElement).addClass('done');
        section.completed = true;

        // update visibility of topic's exercises and actions
        updateVisibilityOfTopicElements(topicIndex);
      }
    }
  }

  // update the UI after a section or topic (with 0 sections) gets skipped
  function sectionSkipped(exerciseElement) {
    var sectionSkippedId;
    if (exerciseElement.length) {
      sectionSkippedId = exerciseElement[0].id;
    }
    else {  // error
      console.log('section ' + $(exerciseElement).selector.split('"')[1] +' not found');
      return;
    }


    var topicIndex = -1;
    $.each(topics, function( ti, topic) {
      if (sectionSkippedId == topic.id) {
        topicIndex = ti;
        topic.topicCompleted = true;
        return false;
      }
      $.each(topic.sections, function( si, section) {
        if (sectionSkippedId == section.id) {
          topicIndex = ti;
          section.skipped = true;
          topic.sectionsSkipped++;
          return false;
        }
      })
      return topicIndex == -1;
    })

    // update the progress bar
    updateTopicProgressBar(topicIndex);
    // update visibility of topic's exercises and actions
    updateVisibilityOfTopicElements(topicIndex);
  }


  transformDOM();
  handleLocationHash();

  // initialize components within tutorial.onInit event
  tutorial.onInit(function() {

    // handle progress events
    tutorial.onProgress(function(progressEvent) {
      if (progressEvent.event === "section_completed")
        sectionCompleted(progressEvent.element);
      else if (progressEvent.event === "section_skipped")
        sectionSkipped(progressEvent.element);
    });

  });

});
