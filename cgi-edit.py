#!/usr/bin/python

import cgi
import os
import cgitb; cgitb.enable()

def main ():
	basename = os.environ.get('PATH_INFO','/')[1:]

	if exists(basename):
		# edit
		pass
	else:
		# new page
		pass


	form = cgi.FieldStorage()


	print_headers()
	print_page()


def exists(basename):
	return True

def print_headers():
	print "Content-type: text/html"
	print

def print_page(new=True, basename=None, ext=None, content=None, error=None):
	if new:
		if basename:
			title = "Creating new page \"%s\"" % basename
			nameform = cgi.encode(basename)
		else:
			title = "Creating new page"
			nameform = '<input name="basename" type="text"/>'

		pageform = '''
			<h3>Page Name </h3>
			%(nameform)s
			<h3>Page Type</h3>
			<input type="radio" name="type" value="!wiki">Wiki page</input>
			<input type="radio" name="type" value="tex">LaTeX page</input>
			<input type="radio" name="type" value="!other">Other, specify extension:
			<input type="text" name="ext" /> '''%{'nameform':nameform}
	else:
		title = "Editing page \"%s\"" % basename
		pageform = '<h3>%(basename)s, a %(type)s' % { 'basename':cgi.enc(basename), 'type':'?'}
	

	form = '''
	<form action="#">
	%(pageform)s
	<h3>Content</h3>
	<textarea name="content">
	%(content)s
	</textarea>
	</form>''' % { 'pageform':pageform, 'content': ''}
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
