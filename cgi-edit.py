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
	'tex':       'Latex File',
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
	global self_uri, repos, filename

	tmpdir = tempfile.mkdtemp('','latexki-cgi-')
	os.chdir(tmpdir)
	
	try:
		basename = os.environ.get('PATH_INFO','/')[1:]
		repos    = os.environ.get('HTTP_LATEXKI_REPOS',None)
		self_uri = os.environ.get('REQUEST_URI')
		assert repos, "Need LATEXKI_REPOS environment variable"
		
		prepare_svn()
		form = Form()

		error = None
		old_rev = 0
		conf_rev = None
		done = False
		log = ''

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
			assert basename.isalnum(), "Please use only alphanumerical page names"

		if ext:
			filename = basename+"."+ext
		else:
			filename = basename



		if not new:
			if 'revision' in form:
				old_rev = update(form['revision'])
			else:
				old_rev = update()

		if 'content' in form:
			assert 'comment' in form, "Commit comment is compulsory"
			log = form['comment']
			new_content = form['content'].replace('\r\n','\n') # is this an HACK?
			file(filename,'w').write(new_content.encode('utf8'))
			if new:
				add()
				(error,new_rev) = commit(log.encode('utf8'))
				if not error:
					done = True
			else:
				if 'conf_rev' in form:
					if 'checked_conflict' in form:
						new_rev = update(form['conf_rev'])
						assert conflict(), "This should be a conflict, strange..."
						file(filename,'w').write(new_content.encode('utf8'))
						resolve()
					else:
						error = "Please do resolve your conflict"

				new_rev = update()
				if conflict():
					conf_rev = new_rev
				else:
					(error,new_rev) = commit(log.encode('utf8'))
					if not error:
						done = True
		
		# do SVN stuff here

		if filename:
			content = file(filename,'r').read().decode('utf8')
		else:
			content = 'Enter new page here'

		print_headers()
		if done:
			print_success(new, basename, ext, new_rev)
		else:
			print_page(new, basename, ext, content, log, old_rev, conf_rev, error)

	finally:
		os.chdir("/")
		shutil.rmtree(tmpdir)

def prepare_svn():
	global client
	client = pysvn.Client()
	zero = pysvn.Revision( pysvn.opt_revision_kind.number, 0 )
	client.checkout(repos, '.',False,zero)

def update(req_rev=None):
	if req_rev:
		rev = pysvn.Revision( pysvn.opt_revision_kind.number, req_rev )
	else:
		rev = pysvn.Revision( pysvn.opt_revision_kind.head )
	
	rev = client.update(filename,False,rev)
	return rev.number

def add():
	client.add(filename)

def conflict():
	return client.status(filename)[0].text_status == pysvn.wc_status_kind.conflicted

def resolve():
	client.resolved(filename)

def commit(log = 'No log message'):
	who = os.environ.get('REMOTE_ADDR','unknown')
	rev = client.checkin(filename, "%s, via wiki: %s" % (who,log) )
	if rev:
		# This looks so like "Either String Int" :-)
		return (None, rev.number)
	else:
		return ("Commit failed :-(", None)
	

def exists(basename):
	files =  client.ls(repos)
	sr = (lambda str: os.path.basename(str))
	matches = filter((lambda e: sr(e['name']) == basename or sr(e['name']).startswith(basename+".")),files)
	if len(matches) == 0:
		return (True, None)
	else:
		assert len(matches) == 1, "More than one file with this basename, fix the repository!"
		if sr(matches[0]['name']) == basename:
			return (False,None)
		else:
			ext = sr(matches[0]['name'])[len(basename)+1:]
			return (False, ext)
		

def print_headers():
	print "Content-type: text/html"
	print

def print_page(new, basename, ext, content, log, rev, conf_rev, error):
	if new:
		if basename:
			title = "Creating new page \"%s\"" % basename
			nameform = esc(basename)
		else:
			title = "Creating new page"
			nameform = '<input name="basename" type="text" size="50"/>'

		pageform = '''
			<h3>Page Name </h3>
			%(nameform)s
			<h3>Page Type</h3>
			<input type="radio" name="type" value="!wiki" checked="checked">Wiki page</input>
			<input type="radio" name="type" value="tex">LaTeX page</input>
			<input type="radio" name="type" value="!other">Other, specify extension:
			<input type="text" name="ext" size="5" /> '''%{'nameform':nameform}
	else:
		title = "Editing page \"%s\" at revision %i" % (basename,rev)
		pageform = '<h3>%(basename)s, a %(type)s' % { 'basename':esc(basename), 'type':ptype(ext)}
	
	errortext = ''
	if error:
		errortext = '''<strong>There was an error:</strong> %s''' % esc(error)
	
	conftext =  ''
	if conf_rev:
		conftext =  '''
			<input type="hidden" name="conf_rev" value="%(conf_rev)i"/>
			<input type="checkbox" name="checked_conflict" value="true">
			  There was a conflict with revision %(conf_rev)i. I hereby confirm that I have resolved the
			  conflict to the best of all <strong>and removed the conflict markers</strong>
			</input><br/>
		''' % {'conf_rev':conf_rev}

	form = '''
	<form action="%(self_uri)s" method="POST">
	%(pageform)s
	<h3>Content</h3>
	<textarea name="content" cols="80" rows="30">%(content)s</textarea>
	<input type="hidden" name="revision" value="%(rev)i"/>
	<h3>Commit log entry</h3>
	Please describe your changes. This is mandatory.<br/>
	<input type="text" name="comment" size="80" value="%(comment)s"/>
	<h3>Commit changes</h3>
	<input type="hidden" name="_charset_"/>
	%(conftext)s
	<button type="submit">Commit changes
	</button> (can take a while, please be patient)
	</form>''' % { 'pageform':pageform, 'content': esc(content), 'self_uri':esc(self_uri),
	               'conftext':conftext, 'rev':rev, 'comment':esc(log)}
	print '''
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
<title>%(title)s</title>
</head>
<boddy>
<h1>%(title)s</h1>
%(error)s
%(form)s
</body>
</html>''' % { 'title': esc(title), 'form': form, 'error': errortext}

def print_success(new, basename, ext, new_rev):
	title = 'Successful commit'
	text = "Sucessfully commited %s (a %s) to revision %i" %( esc(basename), ptype(ext), new_rev)
	print '''
<html>
<head>
<title>%(title)s</title>
</head>
<boddy>
<h1>%(title)s</h1>
%(text)s
</body>
</html>''' % { 'title': title, 'text': text}


def ptype(ext):
	if ext: return FILETYPES.get(ext,"\".%s\" file" % esc(ext))
	else:   return "regular wiki page"


main()
