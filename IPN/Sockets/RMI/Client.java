import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

class Client {
  public static void main(String[] args) {
    try {
      final var host = (args.length < 1) ? null : args[0];
      final var registry = LocateRegistry.getRegistry(host);
      final var stub = (Calculator) registry.lookup("Calculator");
      
      final int sum1 = 5, sum2 = 4;
      System.out.printf("%d + %d = %d\n", sum1, sum2, stub.sum(sum1, sum2));

      final int sub1 = 10, sub2 = 3;
      System.out.printf("%d - %d = %d\n", sub1, sub2, stub.substract(sub1, sub2));

      final int multi1 = 30, multi2 = 4;
      System.out.printf("%d * %d = %d\n", multi1, multi2, stub.multiply(multi1, multi2));

      final int div1 = 10, div2 = 3;
      System.out.printf("%d + %d = %f\n", div1, div2, stub.divide(div1, div2));
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
