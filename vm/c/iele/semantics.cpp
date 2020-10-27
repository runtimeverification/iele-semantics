#include "proto/msg.pb.h"
#include "runtime/header.h"
#include "semantics.h"
#include "init.h"
#include "world.h"
#include "vm.h"
#include <string>

using namespace io::iohk::ethereum::extvm;

bool get_error(mpz_ptr status) {
  return mpz_sgn(status) != 0;
}

std::string get_output_data(list *l) {
  static uint32_t tag = getTagForSymbolName("LblListToInts{}");
  void *arr[1];
  arr[0] = l;
  block* ints = (block*)evaluateFunctionSymbol(tag, arr);
  static uint32_t tag2 = getTagForSymbolName("LblrlpEncodeInts{}");
  arr[0] = ints;
  string* token = (string*)evaluateFunctionSymbol(tag2, arr);
  return std::string(token->data, len(token));
}

uint64_t get_schedule(mpz_ptr number, CallContext *ctx) {
  static uint32_t danse_tag = getTagForSymbolName("LblDANSE'Unds'IELE-CONSTANTS'Unds'{}");
  uint32_t tag = danse_tag;
  return (((uint64_t)tag) << 32) | 1;
}

input_data unpack_input(bool iscreate, std::string data) {
  string* token = makeString(data.c_str(), data.size());
  static uint32_t tag = getTagForSymbolName("LblrlpDecode{}");
  void* arr[1];
  arr[0] = token;
  json* decoded = (json*) evaluateFunctionSymbol(tag, arr);
  string* str = ((stringinj*)decoded->data->hd)->data;
  jsonlist* args = ((json*)decoded->data->tl->hd)->data;
  input_data res;
  res.args = (block*)args;
  if (iscreate) {
    res.code = str;
    res.function = makeString("");
  } else {
    res.function = str;
    res.code = makeString("");
  }
  return res;
}

uint32_t kcellInjTag = getTagForSymbolName("inj{SortIELESimulation{}, SortKItem{}}");
uint32_t unparseByteStack = getTagForSymbolName("LblunparseByteStack{}");
