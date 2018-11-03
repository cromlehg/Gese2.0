package eth.contracts

import java.math.BigInteger
import java.util.Arrays
import java.util.Collections
import org.web3j.abi.TypeReference
import org.web3j.abi.datatypes.Function
import org.web3j.abi.datatypes.Type
import org.web3j.abi.datatypes.generated.Uint256
import org.web3j.crypto.Credentials
import org.web3j.protocol.Web3j
import org.web3j.protocol.core.RemoteCall
import org.web3j.protocol.core.methods.response.TransactionReceipt
import org.web3j.tx.Contract
import org.web3j.tx.TransactionManager
import org.web3j.abi.datatypes.Type

class ERC20(
  contractAddress:    String,
  web3j:              Web3j,
  transactionManager: TransactionManager,
  gasPrice:           BigInteger,
  gasLimit:           BigInteger) extends Contract(
  """
pragma solidity ^0.4.20;

contract ERC20 {

  function totalSupply() public constant returns (uint256);

  function balanceOf(address _owner) public constant returns (uint256 balance);

  function transfer(address _to, uint256 _value) public returns (bool success);

  function transferFrom(address _from, address _to, uint256 _value) public returns (bool success);

  function approve(address _spender, uint256 _value) public returns (bool success);

  function allowance(address _owner, address _spender) public constant returns (uint256 remaining);

}

""",
  contractAddress,
  web3j,
  transactionManager,
  gasPrice,
  gasLimit) {

  def approve(spender: String, value: BigInteger): RemoteCall[TransactionReceipt] =
    executeRemoteCallTransaction(new Function(
      "approve",
      Arrays.asList[Type[_]](
        new org.web3j.abi.datatypes.Address(spender),
        new org.web3j.abi.datatypes.generated.Uint256(value)),
      Collections.emptyList[TypeReference[_]]()))

  def totalSupply(): RemoteCall[BigInteger] =
    executeRemoteCallSingleValueReturn(new Function(
      "totalSupply",
      Arrays.asList[Type[_]](),
      Arrays.asList[TypeReference[_]](new TypeReference[Uint256]() {})), classOf[BigInteger])

  //
  //    public RemoteCall<TransactionReceipt> transferFrom(String _from, String _to, BigInteger _value) {
  //        final Function function = new Function(
  //                "transferFrom",
  //                Arrays.<Type>asList(new org.web3j.abi.datatypes.Address(_from),
  //                new org.web3j.abi.datatypes.Address(_to),
  //                new org.web3j.abi.datatypes.generated.Uint256(_value)),
  //                Collections.<TypeReference<?>>emptyList());
  //        return executeRemoteCallTransaction(function);
  //    }
  //
  def balanceOf(owner: String): RemoteCall[BigInteger] =
    executeRemoteCallSingleValueReturn(new Function(
      "balanceOf",
      Arrays.asList[Type[_]](new org.web3j.abi.datatypes.Address(owner)),
      Arrays.asList[TypeReference[_]](new TypeReference[Uint256]() {})), classOf[BigInteger])

  //
  //    public RemoteCall<TransactionReceipt> transfer(String _to, BigInteger _value) {
  //        final Function function = new Function(
  //                "transfer",
  //                Arrays.<Type>asList(new org.web3j.abi.datatypes.Address(_to),
  //                new org.web3j.abi.datatypes.generated.Uint256(_value)),
  //                Collections.<TypeReference<?>>emptyList());
  //        return executeRemoteCallTransaction(function);
  //    }
  //
  //    public RemoteCall<BigInteger> allowance(String _owner, String _spender) {
  //        final Function function = new Function("allowance",
  //                Arrays.<Type>asList(new org.web3j.abi.datatypes.Address(_owner),
  //                new org.web3j.abi.datatypes.Address(_spender)),
  //                Arrays.<TypeReference<?>>asList(new TypeReference<Uint256>() {}));
  //        return executeRemoteCallSingleValueReturn(function, BigInteger.class);
  //    }
  //
  //    public static RemoteCall<Contract> deploy(Web3j web3j, Credentials credentials, BigInteger gasPrice, BigInteger gasLimit) {
  //        return deployRemoteCall(Contract.class, web3j, credentials, gasPrice, gasLimit, BINARY, "");
  //    }
  //
  //    public static RemoteCall<Contract> deploy(Web3j web3j, TransactionManager transactionManager, BigInteger gasPrice, BigInteger gasLimit) {
  //        return deployRemoteCall(Contract.class, web3j, transactionManager, gasPrice, gasLimit, BINARY, "");
  //    }
  //
  //    public static Contract load(String contractAddress, Web3j web3j, Credentials credentials, BigInteger gasPrice, BigInteger gasLimit) {
  //        return new Contract(contractAddress, web3j, credentials, gasPrice, gasLimit);
  //    }
  //
  //    public static Contract load(String contractAddress, Web3j web3j, TransactionManager transactionManager, BigInteger gasPrice, BigInteger gasLimit) {
  //        return new Contract(contractAddress, web3j, transactionManager, gasPrice, gasLimit);
  //    }
}