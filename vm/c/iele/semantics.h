#ifndef SEMANTICS_H
#define SEMANTICS_H

struct json;
struct stringinj;
struct zinj;
struct inj;
struct mapinj;

struct account {
  blockheader h;
  zinj* acctID;
  zinj* balance;
  inj* code;
  mapinj *storage;
  zinj *nonce;
};

struct log {
  blockheader h;
  mpz_ptr acct;
  list topics;
  string *data;
};

struct input_data {
  string* code;
  block* args;
  string* function;
};

struct accounts;

struct tx_result {
  blockheader h;
  list rets;
  mpz_ptr gas;
  mpz_ptr refund;
  mpz_ptr status;
  list selfdestruct;
  list logs;
  accounts* accounts; 
  list touched;
};

std::string get_output_data(list *);

extern uint32_t unparseByteStack;

#define logData(log) log->data

#endif
