#!/usr/bin/env python3
from calibre.ebooks.metadata.book.serialize import metadata_as_dict
from calibre.gui2.device import *
from calibre.prints import prints
import json
import sys
if len(sys.argv) != 2:
    print("Usage: script_name.py <path_to_books>")
    sys.exit(1)
books_path = sys.argv[1]
device = FOLDER_DEVICE(books_path)
device.set_progress_reporter(prints)
books = device.books()
books_array = [metadata_as_dict(i) for i in books]
print((json.dumps(books_array, default=str)))
