cl-wadler-pprint
================

An implementation of ["A Prettier Printer" by Philip Wadler](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf) in Common Lisp.

TODOs
-----

-	It might make sense to use laziness and/or memoization to speed up pretty-printing.

Notes
-----

### ABCL

In my entrypoint (written in Java), I've got a method:

```java
import java.io.IOException;
import java.util.Scanner;
import org.armedbear.lisp.JavaObject;
import org.armedbear.lisp.Packages;

public static void updateRightMargin() {
	int rightMargin;
	try {
		Process proc = new ProcessBuilder("tput", "cols")
			.inheritIO()
			.redirectOutput(ProcessBuilder.Redirect.PIPE)
			.start();
		rightMargin = new Scanner(proc.getInputStream()).nextInt();
		if(proc.waitFor() != 0) {
			logError("Failed to update *PRINT-RIGHT-MARGIN*.");
			return;
		}
	} catch(InterruptedException ex) {
		logError("Failed to update *PRINT-RIGHT-MARGIN*: " + ex);
		return;
	} catch(IOException ex) {
		logError("Failed to update *PRINT-RIGHT-MARGIN*: " + ex);
		return;
	}

	Packages.findPackage("COMMON-LISP")
		.findAccessibleSymbol("*PRINT-RIGHT-MARGIN*")
		.setSymbolValue(JavaObject.getInstance(rightMargin, true));
}
```

and in `main`, code like:

```java
import sun.misc.Signal;
import sun.misc.SignalHandler;

Signal.handle(new Signal("WINCH"), new SignalHandler() {
	public void handle(Signal sig) {
		Main.updateRightMargin();
	}
});
updateRightMargin();
```
