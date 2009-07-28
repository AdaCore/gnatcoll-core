//#include <pgsql/libpq-fe.h>

typedef void PGconn;
typedef void PGresult;
typedef void Oid;

PGresult* gnatcoll_pqprepare (
   PGconn *conn,
   const char *stmtName,
   const char *query,
   int nParams,
   const Oid *paramTypes)
{
#ifdef HAS_PQPREPARE
   extern PGresult* PQprepare
      (PGconn*,const char*, const char*, int, const Oid*);
   return PQprepare (conn, stmtName, query, nParams, paramTypes);
#else
   return (PGresult*)0;
#endif
}

