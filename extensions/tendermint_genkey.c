/* tendermint_genkey.c -- genrate tendermint keypair vectors in sexp

   Copyright (C) 2020 David Dashyan <mail@davie.li>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include <string.h>
#include <stdio.h>
#include <libguile.h>
#include <sodium.h>

/* Generate ed25519 keypair and return list in form:
   (tendermint-ed25519-key
     (pubkey PUBKEYBYTEVECTOR)
     (privkey PRIVKEYBYTEVECTOR)) */
SCM
tendermint_ed25519_genkey (void)
{
  /* TODO maybe its better to signal error in tendermint_init_guile */
  /* if (sodium_init () == 1) */
  /*     return SCM_BOOL_F; */

  unsigned char pk[crypto_sign_ed25519_PUBLICKEYBYTES];
  unsigned char sk[crypto_sign_ed25519_SECRETKEYBYTES];
  crypto_sign_ed25519_keypair (pk, sk);

  SCM scmpk = scm_c_make_bytevector (crypto_sign_ed25519_PUBLICKEYBYTES);
  SCM scmsk = scm_c_make_bytevector (crypto_sign_ed25519_SECRETKEYBYTES);

  memcpy (SCM_BYTEVECTOR_CONTENTS (scmpk),
          &pk,
          crypto_sign_ed25519_PUBLICKEYBYTES);

  memcpy (SCM_BYTEVECTOR_CONTENTS (scmsk),
          &sk,
          crypto_sign_ed25519_SECRETKEYBYTES);

  SCM sk_list = scm_list_2 (scm_from_locale_symbol ("pubkey"), scmsk);
  SCM pk_list = scm_list_2 (scm_from_locale_symbol ("privkey"), scmpk);
  SCM tm_key = scm_list_3 (scm_from_locale_symbol ("tendermint-ed25519-key"),
                           sk_list,
                           pk_list);
  return tm_key;
}

void
tendermint_init_guile (void)
{
  sodium_init ();
  /* TODO signal error */

  scm_c_define_gsubr ("tendermint-ed25519-genkey",
                      0, 0, 0, tendermint_ed25519_genkey);
  scm_c_export ("tendermint-ed25519-genkey", NULL);
}
