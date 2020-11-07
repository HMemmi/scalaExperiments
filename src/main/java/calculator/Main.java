package calculator;

import currying.callback.Person;

public class Main {
	public static void main(String[] args) {
		Person p = Person.apply("hichem", "zaineb");
		System.out.println(p.name());
	}
}
