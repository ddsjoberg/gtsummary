(function($) {
    $(document).ready(function() {
	%s
	$('#%s').scianimator({
	    'images': [%s],
	    'width': %s,
	    'delay': %s,
	    'loopMode': '%s'%s
	});
	$('#%s').scianimator('play');
    });
})(jQuery);
