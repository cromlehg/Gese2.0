package eth.contracts

import java.math.BigInteger

import org.web3j.tx.ReadonlyTransactionManager

class ERC20Helper(invokerAddress: String, contractAddress: String) extends Web3Service {

  val txManager = new ReadonlyTransactionManager(web3, invokerAddress)

  val erc20 = new ERC20(contractAddress, web3, txManager, BigInteger.valueOf(0), BigInteger.valueOf(0))

  def balanceOf(targetAddress: String) =
    erc20.balanceOf(targetAddress).send().toString

}

object ERC20BalaceOf {

  def apply(invokerAddress: String, contractAddress: String, targetAddress: String): String =
    new ERC20Helper(invokerAddress, contractAddress).balanceOf(targetAddress)

}