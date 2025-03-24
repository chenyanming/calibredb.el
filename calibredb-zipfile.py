#!/usr/bin/env python3

import sys
import json
from calibre.ebooks.oeb.polish.container import get_container

def extract_content(file_path):
    container = get_container(file_path)

    # Extract and print metadata
    metadata = container.mime_map
    # pretty print
    print(json.dumps(metadata, indent=4))

    # print(container.raw_data("text/part0201.html"))

if __name__ == '__main__':
    epub_file = sys.argv[1]
    extract_content(epub_file)
