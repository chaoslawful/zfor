#!/usr/bin/env python
from distutils.core import setup, Extension

zformod = Extension(
            'zfor',
            sources = ['src/zfor.c'],
            include_dirs = ['../libzfor'],
            library_dirs = ['/usr/local/lib', '../libzfor'],
            libraries = ['zfor']
        )

setup(
        name = 'zfor',
        version = '0.1',
        description = 'Python zfor binding',
        author = ['Chris Goffinet'],
        author_email = ['cg@chrisgoffinet.com'],
        packages = [
            'zfor',
            ],
        package_dir = {'zfor' : 'src'},
        ext_modules = [zformod],
    )

# vim:ft=python ts=4 sw=4 et

