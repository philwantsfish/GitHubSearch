package fish.philwants

import java.lang.management.ManagementFactory
import java.net.ServerSocket

import com.typesafe.scalalogging.LazyLogging
import fish.philwants.GitHubClient.{CodeSearch, Repo, CodeSearchResult}
import org.jsoup.Jsoup
import spray.json._
import DefaultJsonProtocol._
import java.io._
import scala.io.Source

object GitHubClient extends LazyLogging with DefaultJsonProtocol {
  case class RepoSearchResult(items: Seq[Repo])
  case class Repo(name: String, full_name: String, html_url: String, url: String )
  case class CodeSearchResult(items: Seq[CodeSearch])
  case class CodeSearch(name: String, html_url: String, path: String, repository: Repo)

  implicit val repoFormat = jsonFormat4(Repo)
  implicit val repoListFormat = jsonFormat1(RepoSearchResult)
  implicit val codeSearchFormat = jsonFormat4(CodeSearch)
  implicit val codeSearchResultFormat = jsonFormat1(CodeSearchResult)

  def uri_search_repos(language: String, per_page: Int, page_num: Int = 1): String = {
    s"https://api.github.com/search/repositories?q=stars:>1+language:${language}&sort=stars&order=desc&per_page=$per_page&page=${page_num}"
  }

  /**
   * Get a page of most popular repositories for a language
   */
  def most_popular_repo_page(language: String, per_page: Int, page_num: Int): Seq[Repo] = {
    val uri = uri_search_repos(language, per_page, page_num)
//    println(uri)
    val doc = Jsoup.connect(uri)
      .ignoreContentType(true)
      .get()
    val json = doc.body().text()
    val result = JsonParser(json).convertTo[RepoSearchResult]
    logger.debug(s"Collected ${result.items.size} repositories")
    result.items
  }

  /**
   * Get a list of the most popular repositories for a language
   */
  def most_popular_repo(language: String, count: Int): Seq[Repo] = {
    // The GitHub API limits the response to 100 repos
    val per_page = 100
    val requests = (count / per_page) + 1
    logger.debug(s"About to send $requests requests for repos")
    (1 until requests).flatMap { i =>
      most_popular_repo_page(language, per_page, i)
    }.take(count)
  }

  def uri_search_code(searchTerm: String, repo: String): String = {
    s"https://api.github.com/search/code?q=$searchTerm+repo:$repo"
  }


  /**
   * Search a repositories code
   */
  def search_repo(repo: Repo, searchTerm: String): CodeSearchResult = {
    val uri = uri_search_code(searchTerm, repo.full_name)
//    println(uri)
    val doc = Jsoup.connect(uri)
      .ignoreContentType(true)
      .get()
    val json = doc.body().text()
    val result = JsonParser(json).convertTo[CodeSearchResult]
    result
  }

  /**
   * Search a list of repositories code getting a result for each one
   *
   * GitHub highly rate limits this API. Perform 10, sleep for a while, loop
   */
  def search_repo_rate_limited(repos: Seq[Repo], searchTerm: String): Seq[CodeSearchResult] = {
    val thirtySeconds = 30 * 1000
    val rateLimitChunk = 10

    repos.grouped(rateLimitChunk).flatMap { chunked_repos =>
      logger.debug(s"Searching repos ...")
      Thread.sleep(thirtySeconds)
      chunked_repos.map { r =>
        search_repo(r, searchTerm)
      }
    }.toSeq
  }
}

object Oracle {
  def printRepoInfo(r: Repo, c: CodeSearchResult): Unit = {
    println("[+] Repo: " + r.full_name)
    println(s"[+] ${c.items.size} hits")
    c.items.filter( hit => !isTestCode(hit) ).foreach { hit =>
      println(s"[+]\t ${hit.html_url}")
    }
  }

  def collect_data(searchTerm: String): Unit = {
    val repos = GitHubClient.most_popular_repo("Java", 200)
    val codeSearches = repos zip GitHubClient.search_repo_rate_limited(repos, searchTerm)

    println(s"[+] Found ${codeSearches.size} repos")
//    codeSearches.foreach { case (r,c) => printRepoInfo(r, c) }

    val json = codeSearches.toJson.prettyPrint

    val pw = new PrintWriter(new File("github_json.json" ))
    pw.write(json)
    pw.close()
  }

  def read_data(): Seq[(Repo, CodeSearchResult)] = {
    val json = Source.fromFile("github_json.json").getLines().mkString
//    println(json)
    JsonParser(json).convertTo[Seq[(Repo, CodeSearchResult)]]
  }

  def isTestCode(c: CodeSearch): Boolean = {
    val splitpath = c.path.split('/')
    splitpath.contains("test") || splitpath.contains("tests")
  }
  def isAllTestCode(c: CodeSearchResult): Boolean = c.items.forall { item => isTestCode(item) }

  def analyze_data(data: Seq[(Repo, CodeSearchResult)]): Unit = {
    val num = data.size
    println(s"Analyzing $num repos")
    val sorted_data: Map[String, Seq[(Repo, CodeSearchResult)]] = data.groupBy { case (r, c) =>
      if (c.items.isEmpty) {
        "Safe"
      } else if (isAllTestCode(c)) {
        "Safe - Test"
      } else if (isAndroid(r)) {
        "Android"
      } else {
        "Unsafe"
      }

    }

    println(s"Safe        -> ${sorted_data("Safe").size}, ${sorted_data("Safe").size.toDouble / num * 100}%")
    println(s"Safe - Test -> ${sorted_data("Safe - Test").size}, ${sorted_data("Safe - Test").size.toDouble / num * 100}%")
    println(s"Android     -> ${sorted_data("Android").size}, ${sorted_data("Android").size.toDouble / num * 100}%")
    println(s"Unsafe      -> ${sorted_data("Unsafe").size}, ${sorted_data("Unsafe").size.toDouble / num * 100}%")

    println("\n\n")

    println("Unsafe repos:")
    sorted_data("Unsafe").foreach { case (r, c) => printRepoInfo(r, c) }

//    println("\n\n")
//    println("Android repos:")
//    sorted_data("Android").foreach { case (r, c) => printRepoInfo(r, c) }

  }

  def isAndroid(r: Repo): Boolean = {
    val uri = "https://github.com/" + r.full_name // + "/blob/master/README.md"
    val readme = Jsoup.connect(uri).get()
    readme.text().contains("android")
  }

  def main(args: Array[String]): Unit = {
    Oracle.collect_data("\"new ObjectInputStream\"")
    val data = Oracle.read_data()
    Oracle.analyze_data(data)

  }
}
