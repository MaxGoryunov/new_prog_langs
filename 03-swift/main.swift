import Foundation

func removeDuplicateWords(from text: String) -> String {
    var visited: Set<String> = []
    var constructed: [String] = []
    for word in text.split(separator: " ") {
        let trimmed = word.trimmingCharacters(in: .whitespacesAndNewlines)
        if !visited.contains(trimmed) {
            visited.insert(trimmed)
            constructed.append(String(trimmed))
        }
    }
    return constructed.joined(separator: " ")
}

func readFromConsole() -> String {
    print("Please enter your text:")
    let input = readLine() ?? ""
    return input
}

func readFromFile() -> String? {
    print("Please enter the path to the .txt file:")
    guard let filePath = readLine() else {
        return nil
    }
    do {
        let contents = try String(contentsOfFile: filePath, encoding: .utf8)
        return contents
    } catch {
        print("Error reading file: \(error.localizedDescription)")
        return nil
    }
}

func writeToFile(_ output: String) {
    let fileName = "output.txt"
    do {
        try output.write(toFile: fileName, atomically: true, encoding: .utf8)
        print("The result has been written to \(fileName)")
    } catch {
        print("Error writing to file: \(error.localizedDescription)")
    }
}

func countWords(in text: String) -> Int {
    let trimmed = text.trimmingCharacters(in: .whitespacesAndNewlines)
    guard !trimmed.isEmpty else {
        return 0
    }
    let words = trimmed.split { $0.isWhitespace }
    return words.count
}

func main() {
    print("Do you want to read input from console or from a .txt file? (Enter 'console' or 'file'):")
    guard let choice = readLine()?.lowercased() else {
        print("Invalid input. Please start again.")
        return
    }
    var inputText: String?
    if choice == "console" {
        inputText = readFromConsole()
    } else if choice == "file" {
        inputText = readFromFile()
    } else {
        print("Invalid choice. Please enter 'console' or 'file'.")
        return
    }
    guard let text = inputText else {
        print("No text was provided.")
        return
    }
    let result = removeDuplicateWords(from: text)
    if choice == "console" {
        print("Result:\n\(result)")
    } else if choice == "file" {
        print("Words in source: \(countWords(in: inputText!))\n")
        print("Words in output: \(countWords(in: result))\n")
        writeToFile(result)
        print("Written to output.txt successfully!\n")
    }
}

main()