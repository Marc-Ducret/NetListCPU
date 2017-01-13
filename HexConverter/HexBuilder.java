import java.io.*;

public class HexBuilder {

	public static void main(String[] args) throws IOException {
		if(args.length < 2) {
			System.err.println("Args format [src] [dest]");
			System.exit(-1);
		}
		BufferedReader read = new BufferedReader(new FileReader(new File(args[0])));
		DataOutputStream out = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(new File(args[1]))));
		for(String l; (l = read.readLine()) != null;) {
			out.writeInt((int) Long.parseLong(l, 16));
		}
		read.close();
		out.close();
		System.out.println("Success");
	}
}
