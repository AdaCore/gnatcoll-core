echo "== DEBUG.LOCATION=yes"
./test .gnatdebug_location_yes

echo ""
echo "== DEBUG.LOCATION=no"
./test .gnatdebug_location_no

echo ""
echo "== DEBUG.ENCLOSING_ENTITY=yes"
./test .gnatdebug_enclosing_entity_yes

echo ""
echo "== DEBUG.ENCLOSING_ENTITY=no"
./test .gnatdebug_enclosing_entity_no
