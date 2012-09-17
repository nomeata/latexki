#!/usr/bin/python
# coding: utf-8
#
# © 2006 Joachim Breitner
#

import cgi
import os
import shutil
import tempfile

import cgitb; cgitb.enable()

import pysvn

def esc(str): return cgi.escape(str, True)

FILETYPES={
    'tex':       'LaTeX Document',
    'part.tex':  'partial LaTeX Document',
    'hs':        'Haskell Code File',
    'py':        'Python Code File',
    'wiki-conf': 'Wiki Configuration File',
}

class Form(cgi.FieldStorage):
    def __getitem__(self,item):
        encoding = self.getfirst('_charset_') or 'UTF-8'
        return self.getfirst(item).decode(encoding)

    def getfirst(self, key, default=None):
        """ Return the first value received."""
        if key in self:
            value = cgi.FieldStorage.__getitem__(self,key)
            if type(value) is type([]):
                return value[0].value
            else:
                return value.value
        else:
            return default

def main ():
    global self_uri, repos, filename, who

    tmpdir = tempfile.mkdtemp('','latexki-cgi-')
    os.chdir(tmpdir)
    
    try:
        basename = os.environ.get('PATH_INFO','/')[1:]
        repos    = os.environ.get('HTTP_LATEXKI_REPOS',None)
        self_uri = os.environ.get('SCRIPT_NAME') + os.environ.get('PATH_INFO','')
        who = os.environ.get('REMOTE_ADDR','unknown').decode('utf8')
        assert repos, "Need HTTP_LATEXKI_REPOS environment variable"
        
        prepare_svn()
        form = Form()

        line_from = None
        line_to   = None
        if 'lines' in form:
            (a,b) = form['lines'].split('-',1)
            line_from = int(a)
            line_to   = int(b)

        error = None
        old_rev = 0
        conf_rev = None
        done = False
        log = ''
        theanswer = '23'
        content = ''

        if 'basename' in form:
            basename = form['basename']
            assert 'type' in form, "Extension choice forgotten"
            ext = form['type']
            if   ext == "!wiki":
                ext = None
            elif ext == "!other":
                assert 'ext' in form, "Extension entry forgotten"
                ext = form['ext']
            (new,old_ext) = exists(basename)
            assert new or ext == old_ext, "File exists with different extension"
        else:
            (new,ext) = exists(basename)
            

        if new and basename:
            dirs = basename.split("/")
            for i in range(0,len(dirs)):
                assert dirs[i].isalnum(), "Please use only alphanumerical page names"
            for i in range(1,len(dirs)):
                subpath = "/".join(dirs[0:i])
                assert not(exists(subpath)[0]), "Directory %s does not exist" % subpath

        if ext:
            filename = basename+"."+ext
        else:
            filename = basename


        dirname  = os.path.dirname(basename)
        if dirname:
            get_directory(dirname)

        if not new:
            if 'revision' in form:
                old_rev = update(form['revision'])
            else:
                old_rev = update()

        if 'content' in form:
            assert 'comment' in form, "Commit comment is compulsory"
            assert 'theanswer' in form, "Calculation answer is compulsory"
            log = form['comment']
            theanswer = form['theanswer']
            content = form['content'].replace('\r\n','\n') # is this an HACK?
            if "http" in log:
                error = "No URLs in the comments, please (Anti-Spam-Measure)"
            elif theanswer != "42":
                error = "Please enter 42 in the anti-spam-box, not %s." % theanswer
            elif log == "USA":
                error = "This is a spammers comments, ignored"
            else:
                if len(content) > 0 and content[-1] != '\n':
                    content += '\n'
                if line_from and line_to:
                    old_content  = file(filename,'r').read().decode('utf8')
                    pre_content  = ''.join(old_content.splitlines(True)[:line_from-1])
                    post_content = ''.join(old_content.splitlines(True)[line_to:])
                    content = pre_content + content + post_content
                
                file(filename,'w').write(content.encode('utf8'))
                if new:
                    add()
                    (error,new_rev) = commit(log)
                    if not error:
                        done = True
                else:
                    if 'conf_rev' in form:
                        if 'checked_conflict' in form:
                            new_rev = update(form['conf_rev'])
                            assert conflict(), "This should be a conflict, strange..."
                            file(filename,'w').write(content.encode('utf8'))
                            resolve()
                        else:
                            error = "Please do resolve your conflict"

                    new_rev = update()
                    if conflict():
                        conf_rev = new_rev
                    else:
                        (error,new_rev) = commit(log)
                        if not error:
                            done = True
            
        if conf_rev:
            line_from = line_to = None # Conflicts and line handling bite

        if not content:
            if filename and not new:
                content = file(filename,'r').read().decode('utf8')
                if line_from and line_to:
                    content = ''.join(content.splitlines(True)[line_from-1 : line_to])
            else:
                content = 'Enter new page here. For LaTeX-Pages, please enter a LaTeX-Document'

        print_headers()
        if done:
            print_success(new, basename, ext, new_rev)
        else:
            print_page(new, basename, ext, content, log, theanswer, old_rev, conf_rev, error, line_from, line_to)

    finally:
        os.chdir("/")
        shutil.rmtree(tmpdir)

def prepare_svn():
    global client
    client = pysvn.Client()
    client.set_default_username((who + u' via wiki').encode('utf8'))
    zero = pysvn.Revision( pysvn.opt_revision_kind.number, 0 )
    client.checkout(repos, '.',False,zero)

def get_directory(dirname):
    global client
    client.update(dirname, False)

def update(req_rev=None):
    if req_rev:
        rev = pysvn.Revision( pysvn.opt_revision_kind.number, req_rev )
    else:
        rev = pysvn.Revision( pysvn.opt_revision_kind.head )
    
    rev_list = client.update(filename,False,rev)
    assert len(rev_list) == 1
    return rev_list[0].number

def add():
    client.add(filename)

def conflict():
    return client.status(filename)[0].text_status == pysvn.wc_status_kind.conflicted

def resolve():
    client.resolved(filename)

def commit(log = u'No log message'):
    msg = log
    rev = client.checkin(filename, msg)
    if rev:
        # This looks so like "Either String Int" :-)
        return (None, rev.number)
    else:
        if client.status(filename)[0].text_status == pysvn.wc_status_kind.normal:
            new_rev = update()
            return (None, new_rev)
        else:
            return ("Commit failed :-(" , None)

def exists(basename):
    dirname = os.path.dirname(basename)
    basebasename = os.path.basename(basename)
    files =  client.ls(repos + "/" + dirname)
    sr = (lambda str: os.path.basename(str))
    matches = filter((lambda e: sr(e['name']) == basebasename or
                                sr(e['name']).startswith(basebasename+".")),files)
    if len(matches) == 0:
        return (True, None)
    else:
        assert len(matches) == 1, "More than one file with this basename, fix the repository!"
        if sr(matches[0]['name']) == basebasename:
            return (False,None)
        else:
            ext = sr(matches[0]['name'])[len(basebasename)+1:]
            return (False, ext)

def print_headers():
    print "Content-type: text/html"
    print

def print_page(new, basename, ext, content, log, theanswer, rev, conf_rev, error, line_from, line_to):
    if new:
        if basename:
            title = u"Creating new page “%s”" % basename
            nameform = esc(basename)
        else:
            title = u"Creating new page"
            nameform = u'<input name="basename" type="text" size="50"/>'
        
        pageform = u'''
            <h3>Page Name </h3>
            %(nameform)s
            <h3>Page Type</h3>
            <input type="radio" name="type" value="!wiki" checked="checked">Wiki page</input>
            <input type="radio" name="type" value="tex">LaTeX document</input>
            <input type="radio" name="type" value="part.tex">Partial LaTeX document</input>
            <input type="radio" name="type" value="!other">Other, specify extension:
            <input type="text" name="ext" size="5" /> '''%{'nameform':nameform}
    else:
        title = u"Editing page “%s” at revision %i" % (basename,rev)
        pageform = u'<h3>%(basename)s, a %(type)s' % { 'basename':esc(basename), 'type':ptype(ext)}
    
    errortext = ''
    if error:
        errortext = u'''<strong>There was an error:</strong> %s''' % esc(error)
    
    conftext =  ''
    if conf_rev:
        conftext =  u'''
            <input type="hidden" name="conf_rev" value="%(conf_rev)i"/>
            <input type="checkbox" name="checked_conflict" value="true">
              There was a conflict with revision %(conf_rev)i. I hereby confirm that I have resolved the
              conflict to the best of all <strong>and removed the conflict markers</strong>
            </input><br/>
        ''' % {'conf_rev':conf_rev}

    linetext = ''
    if line_from and line_to:
        linetext = u'<input type="hidden" name="lines" value="%d-%d"/>' % (line_from, line_to)

    form = u'''
    <form action="%(self_uri)s" method="POST">
    %(pageform)s
    <h3>Content</h3>
    <textarea id="content" name="content" cols="80" rows="30">%(content)s</textarea>
    <input type="hidden" name="revision" value="%(rev)i"/>
    <h3>Commit log entry</h3>
    Please describe your changes. This is mandatory.<br/>
    <input type="text" name="comment" size="80" value="%(comment)s"/>
    <h3>What is 40 + 2?</h3>
    Sorry for the inconvenience. This is mandatory, to prevent spam.<br/>
    <input type="text" name="theanswer" size="80" value="%(theanswer)s"/>
    <h3>Commit changes</h3>
    <input type="hidden" name="_charset_"/>
    %(conftext)s
    %(linetext)s
    <button type="submit">Commit changes
    </button> (can take a while, please be patient)
    </form>''' % { 'pageform':pageform, 'content': esc(content), 'self_uri':esc(self_uri),
                   'conftext':conftext, 'rev':rev, 'comment':esc(log), 'theanswer':esc(theanswer),
                   'linetext':linetext }
    print (u'''
<!doctype html>
<html>
<head>
<meta charset="utf-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
<title>%(title)s</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0" />
<link rel="stylesheet" media="screen" href="/css/screen.css" />
<script src="/js/CodeMirror-2.31/lib/codemirror.js"></script>
<link rel="stylesheet" href="/js/CodeMirror-2.31/lib/codemirror.css">
%(mode)s
</head>
<body class="editlatex">
<h1>%(title)s</h1>
%(error)s
%(form)s
<script type="text/javascript"> 
var editor = CodeMirror.fromTextArea(document.getElementById('content'), {
    lineNumbers: true,
    lineWrapping: true,
    firstLineNumber: %(line_from)d,
});
</script>
</body>
</html>''' % { 'title': esc(title), 'form': form, 'error': errortext,
                'mode': '<script src="/js/CodeMirror-2.31/mode/stex/stex.js"></script>' if ext == 'tex' else  '', 'line_from': 1 if line_from is None else line_from
        }).encode('utf8')


def print_success(new, basename, ext, new_rev):
    title = u'Successful commit'
    text = u'Sucessfully commited <a href="/%(basename)s.html">%(basename)s</a> (a %(ext)s) to revision %(rev)i' % {
        'basename': esc(basename), 'ext': ptype(ext), 'rev': new_rev}
    print '''
<html>
<head>
<title>%(title)s</title>
</head>
<body>
<h1>%(title)s</h1>
%(text)s
</body>
</html>''' % { 'title': title, 'text': text}


def ptype(ext):
    if ext: return FILETYPES.get(ext,u"“.%s” file" % esc(ext))
    else:   return u"regular wiki page"


main()
# vim:ts=4:sw=4:expandtab:smarttab
