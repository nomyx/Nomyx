$(function(){
    if (embed) return;
    $(window).resize(resizeDocs);
    newDocs();
});

function resizeDocs()
{
    $("#body .doc").each(function(){
        // If a segment is open, it should remain open forever
        var $this = $(this);
        var toosmall = ($.support.preWrap && $this.hasClass("newline")) ||
                       ($this.height() < $this.children().height());
        if (toosmall && !$this.hasClass("open"))
            $this.addClass("shut");
        else if (!toosmall && $this.hasClass("shut"))
            $this.removeClass("shut");
    });
}

function newDocs()
{
    resizeDocs();
    $("#body .doc").click(function(){
        var $this = $(this);
        if ($this.hasClass("open") || $this.hasClass("shut"))
            $this.toggleClass("open").toggleClass("shut");
    });
}

