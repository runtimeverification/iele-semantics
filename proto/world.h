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
};

#endif
