set -e
find ../parse/parse_examples/ -type f -name \*.mini -exec ./imini {} \;
find ../parse/parse_funexamples/ -type f -name \*.fmini -exec ./imini {} \;
