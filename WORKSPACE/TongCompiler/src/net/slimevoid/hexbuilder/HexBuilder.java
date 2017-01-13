import java.io.*;

public class HexBuilder {

	public static void main(String[] args) {
		if(args.length < 2) {
			System.err.println("Args format [src] [dest]");
		}
		BufferedReader read = new BufferedReader(new FileReader(new File(args[0])));
		DataOutputStream out = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(new File(args[1]))));
		for(String l; (l = read.readLine()) != null;) {
			out.writeInt(Integer.parseInt(l, 16));
		}
		read.close();
		out.close();
		System.out.println("Success");
	}
}
