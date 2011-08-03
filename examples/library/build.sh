cd obj

# Create the database from the schema (remove the old one, just in case)
rm -f library.db
gnatcoll_db2ada -dbtype=sqlite -dbname=library.db -dbmodel=../dbschema.txt -createdb

# Generate the Ada API
gnatcoll_db2ada -api=Database -dbmodel=../dbschema.txt

cd ..

# -m switch is so that gnatmake does not use timestamps but checksums
# for dependencies (otherwise since we regenerate the files every time
# we also recompile every time)
gnatmake -q -m -Pdefault.gpr

# Run the executable
./obj/library
