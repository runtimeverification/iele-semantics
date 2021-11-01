#ifndef WORLD_H
#define WORLD_H

std::string of_z_width(unsigned width, mpz_ptr);
std::string of_z(mpz_ptr);
mpz_ptr to_z_unsigned(std::string);
mpz_ptr to_z(std::string);

class World {
public:
  static io::iohk::ethereum::extvm::Account* get_account(std::string acct);
  static io::iohk::ethereum::extvm::StorageData* get_storage_data(std::string acct, std::string index);
  static io::iohk::ethereum::extvm::Code* get_code(std::string acct);
  static io::iohk::ethereum::extvm::Blockhash* get_blockhash(int offset);
  static io::iohk::ethereum::extvm::Balance* verify_inclusion_and_get_balance(std::string stateTrieRoot, std::string ethAddress, std::string inclusionProof);
  static io::iohk::ethereum::extvm::Address* bech_32_to_address(std::string addressStr);
  static io::iohk::ethereum::extvm::AmountBurned* verify_pob(std::string proof);
};

#endif
