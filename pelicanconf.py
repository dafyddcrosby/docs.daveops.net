# -*- coding: utf-8 -*-
AUTHOR = u'David Crosby'
SITENAME = u"DaveOps.net"
SITEURL = ''
TIMEZONE = "America/Edmonton"

PATH = 'posts/'
DELETE_OUTPUT_DIRECTORY = True
THEME = 'notmyidea'
TYPOGRIFY = True

DEFAULT_DATE = (2015, 02, 15, 0, 0, 0)
DIRECT_TEMPLATES = ('index', 'archives', 'search', 'tags')

PDF_GENERATOR = False
REVERSE_CATEGORY_ORDER = True
REVERSE_ARCHIVE_ORDER = True
DEFAULT_PAGINATION = 5
DEFAULT_CATEGORY = 'Articles'

# URL patterns
ARTICLE_URL = '{slug}/'
ARTICLE_SAVE_AS = '{slug}/index.html'
PAGE_URL = '{slug}/'
PAGE_SAVE_AS = '{slug}/index.html'
ARCHIVES_URL = 'archives/'
ARCHIVES_SAVE_AS = 'archives/index.html'
CATEGORY_URL = 'category/{slug}'
CATEGORY_SAVE_AS = 'category/{slug}/index.html'
CATEGORIES_URL = 'categories/'
CATEGORIES_SAVE_AS = 'categories/index.html'
TAG_URL = 'tag/{slug}/'
TAG_SAVE_AS = 'tag/{slug}/index.html'
TAGS_URL = 'tags/'
TAGS_SAVE_AS = 'tags/index.html'
AUTHOR_SAVE_AS = False
AUTHORS_SAVE_AS = False

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None

MENUITEMS = [
    ('About', '/about/'),
    ('Tags', '/tags/'),
    ('Archives', '/archives/')
]

# Blogroll
LINKS = (('Kernel Who?', 'https://kernelwho.wordpress.com'),
         ("David T. Crosby", 'http://dafyddcrosby.com'),
         ('Lonesome Cosmonaut', "http://lonesomecosmonaut.com/"),)

from os.path import expanduser
PLUGIN_PATH = 'plugins/'
PLUGINS = [
    'assets',
    'gzip_cache',
    'related_posts',
    'sitemap',
    'tipue_search',
]

STATIC_PATHS = [
]

# A list of files to copy from the source to the destination
EXTRA_PATH_METADATA = {
}
