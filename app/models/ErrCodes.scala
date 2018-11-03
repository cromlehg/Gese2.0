package models

import controllers.AppConstants

object ErrCodes {

  val EC_EMAIL_NOT_VERIFIED = 0

  val STR_EMAIL_NOT_VERIFIED = "You should confirm registration by email"

  val EC_PASSWORD_NOT_SET = 1

  val STR_PASSWORD_NOT_SET = "Password not set"

  val EC_PASSWORD_MIN_LENGTH = 2

  val STR_PASSWORD_MIN_LENGTH = "Password length should be more than " + AppConstants.PWD_MIN_LENGTH + " symbols"

  val EC_ARTICLES_NOT_ALLOWED = 3

  val STR_ARTICLES_NOT_ALLOWED = "Artciles not allowed now!"

  val EC_PRODUCTS_NOT_ALLOWED = 4

  val STR_PRODUCTS_NOT_ALLOWED = "Products not allowed now!"

  val EC_REVIEWS_NOT_ALLOWED = 5

  val STR_REVIEWS_NOT_ALLOWED = "Reviews not allowed now!"

  val EC_ITEM_NOT_EXISTS = 6

  val STR_ITEM_NOT_EXISTS = "Item not exists!"

  val EC_ACCOUNT_LOCKED = 7

  val STR_ACCOUNT_LOCKED = "Account blocked!"

}
