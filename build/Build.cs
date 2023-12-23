// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

using System.Collections.Generic;
using System.Linq;
using Nuke.Common;
using Nuke.Common.CI.GitHubActions;
using Nuke.Common.Execution;
using Nuke.Common.IO;
using Nuke.Common.ProjectModel;
using Nuke.Common.Tooling;
using Nuke.Common.Tools.DotNet;
using Nuke.Common.Utilities.Collections;
using Nuke.Components;
using Vipentti.Nuke.Components;
using static Nuke.Common.Tools.DotNet.DotNetTasks;
using static Vipentti.Nuke.Components.StandardNames;

[ExtendedGitHubActions(
    "pull-request",
    GitHubActionsImage.WindowsServer2019,
    GitHubActionsImage.UbuntuLatest,
    GitHubActionsImage.MacOsLatest,
    OnPullRequestBranches = [ MainBranch, DevelopBranch ],
    OnPushBranches = [ MainBranch, DevelopBranch ],
    PublishArtifacts = false
    // FetchDepth = 0 // fetch full history
    , SetupDotnetVersions = [ "8.x", ]
    , InvokedTargets = [
        nameof(ITest.Test),
        nameof(IUseLinters.InstallLinters),
        nameof(IUseLinters.Lint),
    ])]
[DisableDefaultOutputForHost<Terminal>(DefaultOutput.Logo)]
class Build :
    NukeBuild,
    IUseCsharpier,
    IUseFantomas,
    IUseCustomLinters,
    IHazSolution,
    ITest,
    ICompile,
    IPackSpecificPackagesWithoutTesting
{
    public T From<T>()
        where T : INukeBuild => (T)(object)this;

    public static int Main() => Execute<Build>(x => x.From<ICompile>().Compile);

    public Solution CurrentSolution => From<IHazSolution>().Solution;

    public bool IsMacOs =>
        System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.OSX);

    public bool IsWindows =>
        System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows);

    public IEnumerable<Project> UnitTests =>
        CurrentSolution.GetAllProjects("*UnitTests");

    public IEnumerable<Project> ExecutionTests =>
        CurrentSolution.GetAllProjects("*ExecutionTests");

    // Run only unit tests in CI because execution tests take a while.
    public IEnumerable<Project> TestProjects =>
        IsServerBuild ? UnitTests : UnitTests.Concat(ExecutionTests);

    bool IUseCsharpier.UseGlobalTool => false;
    bool IUseFantomas.UseGlobalTool => false;

    IEnumerable<AbsolutePath> IUseFantomas.DirectoriesToFormat => new[]
    {
        RootDirectory / "src",
        RootDirectory / "tests",
    };

    IEnumerable<IProvideLinter> IUseCustomLinters.Linters => new IProvideLinter[]
    {
        From<IUseCsharpier>().Linter,
        From<IUseFantomas>().Linter,
    };

    // csharpier-ignore
    public Target RestoreTools => _ => _
        .Before<IRestore>(it => it.Restore)
        .DependentFor<IUseCustomLinters>(it => it.InstallLinters)
        .Executes(() => DotNetToolRestore())
        ;

    public SolutionFolder SrcFolder => CurrentSolution.GetSolutionFolder("src");

    IEnumerable<Project> ProjectsToPack => [
        SrcFolder.GetProject("Visp.Common"),
        SrcFolder.GetProject("Visp.Runtime.Library"),
    ];

    IEnumerable<string> TargetFrameworks => ["net8.0"];

    IEnumerable<(Project Project, string Framework)> ICompile.PublishConfigurations =>
        from project in ProjectsToPack
        from framework in TargetFrameworks
        select (project, framework);

    IEnumerable<Project> IPackSpecificPackagesWithoutTesting.ProjectsToPack => ProjectsToPack;

    public AbsolutePath PackagePath => From<IPack>().PackagesDirectory;

    Configure<DotNetTestSettings> ITest.TestSettings => _ => _
        .SetProcessEnvironmentVariable("VISP_FS_RUNTIME_PACKAGE_PATH", PackagePath)
        .SetProcessEnvironmentVariable("VISP_FS_COMMON_PACKAGE_PATH", PackagePath)
        .SetProcessEnvironmentVariable("VISP_FS_PACKAGE_PATH", PackagePath)
        .SetProcessEnvironmentVariable("VISP_FS_PACKAGE_FEED_PATH", PackagePath / "feed")
        .SetProcessEnvironmentVariable("VISP_FS_LIB_PATH", RootDirectory / "visp" / "lib")
        ;

    Target ITest.Test => _ => _
        .DependsOn<IPackSpecificPackagesWithoutTesting>(x => x.Pack)
        .Executes(() => From<ITest>().TestResultDirectory.CreateOrCleanDirectory())
        .Inherit<ITest>()
        ;

    Configure<DotNetTestSettings, Project> ITest.TestProjectSettings =>
        (_, v) =>
            _.RemoveLoggers($"trx;LogFileName={v.Name}.trx")
                .AddLoggers($"trx;LogFilePrefix={v.Name}");

    // specifying TargetsForTfmSpecificContentInPackage because of
    // https://github.com/dotnet/fsharp/issues/12320#issuecomment-1059791494

    // csharpier-ignore
    Configure<DotNetPublishSettings> ICompile.PublishSettings => _ => _
        .AddProperty("TargetsForTfmSpecificContentInPackage", "");

    // csharpier-ignore
    Configure<DotNetPackSettings> IPack.PackSettings => _ => _
        .AddProperty("TargetsForTfmSpecificContentInPackage", "");
}

public interface IUseCustomLinters : INukeBuild
{
    IEnumerable<IProvideLinter> Linters { get; }

    // csharpier-ignore
    Target InstallLinters => _ => _
        .Before(Lint)
        .Executes(() =>
        {
            var lintSuccess = true;
            foreach (var item in Linters)
            {
                try
                {
                    item.InstallLinter();
                }
                catch
                {
                    lintSuccess = false;
                }
            }

            Assert.True(lintSuccess);
        });

    // csharpier-ignore
    Target Lint => _ => _
        .AssuredAfterFailure()
        .TryAfter<ICompile>(x => x.Compile)
        .Executes(() =>
        {
            var lintSuccess = true;
            foreach (var item in Linters)
            {
                try
                {
                    item.ExecuteLinter();
                }
                catch
                {
                    lintSuccess = false;
                }
            }

            Assert.True(lintSuccess);
        });
}

public interface IPackSpecificPackagesWithoutTesting : IPack
{
    IEnumerable<Project> ProjectsToPack { get; }

    [Parameter]
    bool CleanPackagesDirectory => true;

    IEnumerable<string> TargetFrameworks => ["net8.0"];

    IEnumerable<(Project Project, string Framework)> ICompile.PublishConfigurations =>
        from project in ProjectsToPack
        from framework in TargetFrameworks
        select (project, framework);

    // csharpier-ignore
    Target IPack.Pack => _ => _
        .DependsOn<ICompile>(x => x.Compile)
        .Produces(PackagesDirectory / "*.nupkg")
        .Executes(() =>
        {
            if (CleanPackagesDirectory)
            {
                PackagesDirectory.CreateOrCleanDirectory();
            }

            DotNetPack(
                _ =>
                    _.Apply(PackSettingsBase)
                        .Apply(PackSettings)
                        .CombineWith(ProjectsToPack, (_, v) => _.SetProject(v))
            );

            ReportSummary(_ => _.AddPair("Packages", PackagesDirectory.GlobFiles("*.nupkg").Count.ToString()));

            Nuke.Common.Tools.NuGet.NuGetTasks.NuGet(
                $"init {PackagesDirectory} {PackagesDirectory / "feed"} -Expand"
                , logInvocation: true
            );
        });
}

public interface IUseFantomas : INukeBuild
{
    bool UseGlobalTool { get; }

    IEnumerable<AbsolutePath> DirectoriesToFormat => new[] { RootDirectory };

    // csharpier-ignore
    Target CheckFantomas => _ => _
        .Executes(() => RunFantomas(check: true));

    // csharpier-ignore
    Target InstallFantomas => _ => _
        .OnlyWhenDynamic(() => UseGlobalTool)
        .Executes(ExecuteInstallGlobalFantomas);

    // csharpier-ignore
    Target FormatFantomas => _ => _
        .TryBefore<IUseDotNetFormat>(x => x.Format)
        .TryDependentFor<IUseDotNetFormat>(x => x.Format)
        .Executes(() => RunFantomas(check: false));

    sealed IProvideLinter Linter =>
        new LinterDelegate(ExecuteInstallGlobalFantomas, () => RunFantomas(check: true));

    sealed void ExecuteInstallGlobalFantomas()
    {
        if (UseGlobalTool)
        {
            DotNetToolUpdate(_ => _.SetGlobal(true).SetPackageName("fantomas"));
        }
    }

    sealed void RunFantomas(bool check)
    {
        var toolname = UseGlobalTool ? "fantomas" : "tool run fantomas";

        DirectoriesToFormat.ForEach(RunFormat);

        void RunFormat(AbsolutePath path)
        {
            DotNet(
                arguments: $"{toolname} {path}" + (check ? " --check" : ""),
                logInvocation: true
            );
        }
    }
}
