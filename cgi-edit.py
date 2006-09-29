#!/usr/bin/python

import cgi
import os
import shutil
import tempfile

import cgitb; cgitb.enable()

import pysvn

def esc(str): return cgi.escape(str)

FILETYPES={
	'tex':'Latex File',
	'hs': 'Haskell Code File',
	'py': 'Python Code File',
}

def main ():
	global self_uri, repos, client

	tmpdir = tempfile.mkdtemp('','latexki-cgi-')
	os.chdir(tmpdir)
	
	try:
		basename = os.environ.get('PATH_INFO','/')[1:]
		repos    = os.environ.get('LATEXKI_REPOS',None)
		self_uri = os.environ.get('REQUEST_URI')
		assert repos, "Need LATEXKI_REPOS environment variable"
		
		client = pysvn.Client()

		(new,ext) = exists(basename)

		if ext:
			filename = basename+"."+ext
		else:
			filename = basename

		prepare_svn()

		form = cgi.FieldStorage()

		if not new:
			if 'rev' in form:
				old_rev = checkout(filename, form.getfirst('rev'))
			else:
				old_rev = checkout(filename, None)

		if 'content' in form:
			new_content = form.getfirst('content')
			file(filename,'w').write(new_content)

		# do SVN stuff here

		content = file(filename,'r').read()

		print_headers()
		print_page(new, basename, ext, content, old_rev)

	finally:
		os.chdir("/")
		#shutil.rmtree(tmpdir)

def prepare_svn():
	zero = pysvn.Revision( pysvn.opt_revision_kind.number, 0 )
	client.checkout(repos, '.',False,zero)

def checkout(file, req_rev=None):
	if req_rev:
		rev = pysvn.Revision( pysvn.opt_revision_kind.number, req_rev )
	else:
		rev = pysvn.Revision( pysvn.opt_revision_kind.head )
	
	rev = client.update(file,False,rev)
	return rev.number

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
		
	assert False, str(we)
	return (True,None)

def print_headers():
	print "Content-type: text/html"
	print

def print_page(new=True, basename=None, ext=None, content=None, rev=None, error=None):
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
			<input type="radio" name="type" value="!wiki">Wiki page</input>
			<input type="radio" name="type" value="tex">LaTeX page</input>
			<input type="radio" name="type" value="!other">Other, specify extension:
			<input type="text" name="ext" /> '''%{'nameform':nameform}
	else:
		if ext: type = FILETYPES.get(ext,"\".%s\" file" % esc(ext))
		else:   type = "regular wiki page"

		title = "Editing page \"%s\" at revision %i" % (basename,rev)
		pageform = '<h3>%(basename)s, a %(type)s' % { 'basename':esc(basename), 'type':type}
	

	form = '''
	<form action="%(self_uri)s" method="POST">
	%(pageform)s
	<h3>Content</h3>
	<textarea name="content" cols="80" rows="30">%(content)s</textarea>
	<input type="hidden" name="revision" value="%(rev)i"/>
	<h3>Commit changes</h3>
	<button type="submit">Commit changes
	</button>
	</form>''' % { 'pageform':pageform, 'content': esc(content), 'self_uri':esc(self_uri), 'rev':rev}
	print '''
<html>
<head>
<title>%(title)s</title>
</head>
<boddy>
<h1>%(title)s</h1>
%(form)s
</body>
</html>''' % { 'title': cgi.escape(title), 'form': form}


main()
