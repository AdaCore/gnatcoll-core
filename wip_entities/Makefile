
MODEL=dbschema.txt
INITIAL=initialdata.txt

DB2ADA=../src/obj/gnatcoll_db2ada -output generated -dbmodel=${MODEL}
PARSE=./obj/test_entities --noruntime ../../gps/gps.gpr
PARSE=./obj/test_entities ../../gps/gps.gpr

all:
	@mkdir -p generated

	@# Create a dummy database, just to be able to extract the initial
	@# data from it. ??? Should be doable directly from initialdata.txt

	@${DB2ADA} -dbtype=sqlite -dbname=:memory: \
		-createdb \
		-api GNATCOLL.ALI.Database \
		-load=${INITIAL} \
		-enum "f2f_kind,id,name,F2F_,Integer"  \
		-enum "e2e_kind,id,name,E2E_,Integer" 

	@gprbuild -m -j0 -p -q -Pentities.gpr

run_test:
	rm -f entities.db
	$(PARSE)
	@for table in entities files e2e entity_refs f2f; do \
	   echo "select count(*) as '$$table' from $$table;" \
		   | sqlite3 -line entities.db; \
	done
	@ls --block-size=1 -s entities.db

run_update:
	@rm -f entities.db
	@$(PARSE)

	@for table in entities files e2e entity_refs f2f; do \
	   echo "select count(*) as '$$table' from $$table;" \
		   | sqlite3 -line entities.db; \
	done
	@echo "size=" `ls --block-size=1 -s entities.db`	
	@cp -f entities.db entities.db.first

	@# No file modified, but we still copy the database to memory
	@# temporarily
	@echo ""
	@echo "===== No file modified (forcing copy to memory) ====="
	@$(PARSE) --tmpdb :memory:

	@# No file modified and we only manipulate db on disk
	@echo ""
	@echo "===== No file modified (automatic db) ====="
	@$(PARSE)

	@# Updating one ALI file
	@echo ""
	@echo "===== Updating gps-kernel.ali (automatic db) ====="
	@touch ../../kernel/obj/gps-kernel.ali
	@$(PARSE)

	@# Updating all ALI files
	@echo ""
	@echo "==== Updating all LI files (automatic db) ===="
	@find ../../ -name '*.ali' -exec touch {} \;
	@$(PARSE)
	@for table in entities files e2e entity_refs f2f; do \
	   echo "select count(*) as '$$table' from $$table;" \
		   | sqlite3 -line entities.db; \
	done

test1: force
	rm -f entities.db
	gnatmake -q -Ptest1/test1.gpr
	@$(PARSE)

quiet:
	@rm -f entities.db log
	@unset ADA_DEBUG_FILE; ./obj/test_entities ../../gps/gps.gpr

run_postgresql_test:
	dropdb entities.db
	createdb entities.db
	time ./obj/test_entities --postgres ../../gps/gps.gpr

profile:
	@sync ; echo 3 | sudo tee /proc/sys/vm/drop_caches >/dev/null
	@echo "===== Flushing disk caches ====="
	@rm -f entities.db
	@./obj/test_entities

	@echo "===== Files in disk cache ======"
	@rm -f entities.db
	@./obj/test_entities

	#@echo "===== Start with an existing empty db ===="
	#@rm -f entities.db
	#@${DB2ADA} -dbtype=sqlite -dbname=entities.db -createdb -load=${INITIAL}
	#@./obj/test_entities

doc:
	@gnatcoll_db2ada -dbmodel=${MODEL} -dot
	@ps2pdf -sPAGESIZE=a4 schema.ps
	@${RM} schema.ps schema.dot
	@echo "Created schema.pdf"
	xdg-open schema.pdf

force:
